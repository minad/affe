;;; affe.el --- Asynchronous Fuzzy Finder for Emacs -*- lexical-binding: t -*-

;; Author: Daniel Mendler
;; Maintainer: Daniel Mendler
;; Created: 2021
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (consult "0.7"))
;; Homepage: https://github.com/minad/affe

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Asynchronous Fuzzy Finder for Emacs

;;; Code:

(require 'consult)
(require 'server)
(eval-when-compile (require 'subr-x))

(defgroup affe nil
  "Asynchronous Fuzzy Finder for Emacs."
  :group 'convenience
  :prefix "affe-")

(defcustom affe-count 50
  "Number of matches the backend should return."
  :type 'integer)

(defcustom affe-find-command
  "find -not ( -wholename */.* -prune ) -type f"
  "Find file command."
  :type 'string)

(defcustom affe-grep-command
  "rg --null --line-buffered --color=never --max-columns=1000 --no-heading --line-number -v \"^$\" ."
  "Grep command."
  :type 'string)

(defcustom affe-regexp-function #'affe-default-regexp
  "Transformation function from input string to list of regexps."
  :type 'function)

(defcustom affe-highlight-function #'affe-default-highlight
  "Highlighting function taking the input string and the list of matches."
  :type 'function)

(defvar affe--grep-history nil)
(defvar affe--find-history nil)

(defun affe-default-regexp (pattern)
  "Default PATTERN regexp transformation function."
  (mapcan (lambda (word)
            (condition-case nil
                (progn (string-match-p word "") (list word))
              (invalid-regexp nil)))
          (split-string pattern nil t)))

(defun affe-default-highlight (_ cands)
  "Default highlighting function for CANDS."
  cands)

(defun affe--send (name expr &optional filter)
  "Send EXPR to server NAME and call CALLBACK with result."
  (let* ((result)
         (proc (make-network-process
                :name name
                :noquery t
                :filter filter
                :coding 'raw-text-unix
                :family 'local
                :service (expand-file-name name server-socket-dir))))
    (process-send-string
     proc
     (format "-eval %s \n" (server-quote-arg (prin1-to-string expr))))
    proc))

(defun affe--passthrough-all-completions (str table pred _point)
  "Passthrough completion function.
See `completion-all-completions' for the arguments STR, TABLE, PRED and POINT."
  (let ((completion-regexp-list))
    (funcall affe-highlight-function str (all-completions "" table pred))))

(defun affe--passthrough-try-completion (_str table pred _point)
  "Passthrough completion function.
See `completion-try-completion' for the arguments STR, TABLE, PRED and POINT."
  (let ((completion-regexp-list))
    (and (try-completion "" table pred) t)))

(defun affe--async (async cmd)
  "Create asynchrous completion function from ASYNC with backend CMD."
  (let ((proc)
        (last-input)
        (name (make-temp-name "affe-")))
    (lambda (action)
      (pcase action
        ((pred stringp)
         (unless (or (equal "" action) (equal action last-input))
           (setq last-input action)
           (ignore-errors (delete-process proc))
           (setq proc (affe--send
                       name
                       `(affe-backend-filter ,affe-count ,@(funcall affe-regexp-function action))
                       (lambda (_proc out)
                         (dolist (line (split-string out "\n"))
                           (cond
                            ((equal "-affe-flush" line)
                             (funcall async 'flush))
                            ((string-prefix-p "-affe-match " line)
                             (funcall async (list (substring line 12)))))))))))
        ('destroy
         (ignore-errors (delete-process proc))
         (affe--send name '(kill-emacs))
         (funcall async 'destroy))
        ('setup
         (funcall async 'setup)
         (setq-local completion-styles-alist
                     (cons
                      (list 'affe--passthrough
                            #'affe--passthrough-try-completion
                            #'affe--passthrough-all-completions
                            "")
                      completion-styles-alist)
                     completion-styles '(affe--passthrough)
                     completion-category-defaults nil
                     completion-category-overrides nil)
         (call-process
          (file-truename
           (expand-file-name invocation-name
                             invocation-directory))
          nil nil nil "-Q" (concat "--daemon=" name)
          "-l" (locate-library "affe-backend"))
         (affe--send name `(affe-backend-start ,@(split-string-and-unquote cmd))))
        (_ (funcall async action))))))

(defun affe--read (prompt dir &rest args)
  "Asynchronous selection function with PROMPT in DIR.
ARGS are passed to `consult--read'."
  (let* ((prompt-dir (consult--directory-prompt prompt dir))
         (default-directory (cdr prompt-dir)))
    (apply #'consult--read
           `(,@args :prompt ,(car prompt-dir)
                    :sort nil
                    :require-match t))))

;;;###autoload
(defun affe-grep (&optional dir initial)
  "Fuzzy grep in DIR with optional INITIAL input."
  (interactive)
  (affe--read
   "Fuzzy grep" dir
   (thread-first (consult--async-sink)
     (consult--async-refresh-timer 0.05)
     (consult--async-transform consult--grep-matches)
     (affe--async affe-grep-command))
   :initial initial
   :history '(:input affe--grep-history)
   :category 'consult-grep
   :add-history (thing-at-point 'symbol)
   :lookup #'consult--lookup-cdr
   :title #'consult--grep-title
   :state (consult--grep-state)))

;;;###autoload
(defun affe-find (&optional dir initial)
  "Fuzzy find in DIR with optional INITIAL input."
  (interactive)
  (find-file
   (affe--read
    "Fuzzy find" dir
    (thread-first (consult--async-sink)
      (consult--async-refresh-timer 0.05)
      (consult--async-map (lambda (x) (string-remove-prefix "./" x)))
      (affe--async affe-find-command))
    :history '(:input affe--find-history)
    :initial initial
    :category 'file
    :add-history (thing-at-point 'filename))))

(provide 'affe)
;;; affe.el ends here
