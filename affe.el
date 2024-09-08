;;; affe.el --- Asynchronous Fuzzy Finder for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Daniel Mendler

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Created: 2021
;; Version: 0.8
;; Package-Requires: ((emacs "28.1") (consult "1.7"))
;; Homepage: https://github.com/minad/affe
;; Keywords: matching, files, completion

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Asynchronous Fuzzy Finder for Emacs

;;; Code:

(require 'consult)
(require 'server)
(eval-when-compile (require 'subr-x))

(defgroup affe nil
  "Asynchronous Fuzzy Finder for Emacs."
  :link '(url-link :tag "Homepage" "https://github.com/minad/affe")
  :link '(emacs-library-link :tag "Library Source" "affe.el")
  :group 'files
  :group 'minibuffer
  :prefix "affe-")

(defcustom affe-count 20
  "Number of matches the backend should return."
  :type 'natnum)

(defcustom affe-find-command
  "rg --color=never --files"
  "Find file command."
  :type 'string)

(defcustom affe-grep-command
  "rg --null --color=never --max-columns=1000 --no-heading --line-number -v ^$"
  "Grep command."
  :type 'string)

(defcustom affe-regexp-compiler
  consult--regexp-compiler
  "Affe regular expression compiler."
  :type 'function)

(defvar affe--grep-history nil)
(defvar affe--find-history nil)

(defun affe--connect (name callback)
  "Send EXPR to server NAME and call CALLBACK with result."
  (let ((rest ""))
    (make-network-process
     :name name
     :noquery t
     :coding 'utf-8
     :filter
     (lambda (_ out)
       (let ((lines (split-string out "\n")))
         (if (not (cdr lines))
             (setq rest (concat rest (car lines)))
           (setcar lines (concat rest (car lines)))
           (let ((last (last lines 2)))
             (setq rest (cadr last))
             (setcdr last nil))
           (funcall callback lines))))
     :sentinel
     (lambda (&rest _)
       (unless (equal rest "")
         (funcall callback (list rest))))
     :family 'local
     :service (expand-file-name name server-socket-dir))))

(defun affe--send (proc expr)
  "Send EXPR to PROC."
  (process-send-string
   proc
   (let ((print-escape-newlines t))
     (concat (prin1-to-string expr) "\n"))))

(defun affe--async (async cmd &optional regexp)
  "Create asynchronous completion function from ASYNC.
CMD is the backend command.
REGEXP is the regexp which restricts the substring to match against."
  (let* (proc regexps highlight
         (indicator) (indicator-total 0) (indicator-done) (indicator-active)
         (backend (or (locate-library "affe-backend")
                      (error "Could not locate the library `affe-backend.el'")))
         (name (make-temp-name "affe-"))
         (callback
          (lambda (lines)
            (dolist (line lines)
              (pcase (read line)
                (`(log ,msg)
                 (with-current-buffer (get-buffer-create " *affe*")
                   (insert msg)))
                ('flush (funcall async 'flush))
                (`(producer ,total ,done)
                 (setq indicator-total total indicator-done done))
                (`(search ,active)
                 (setq indicator-active active))
                (`(match ,prefix ,match ,suffix)
                 (when highlight
                   (funcall highlight match))
                 (funcall async (list (concat prefix match suffix))))))
            (overlay-put indicator 'display
                         (format " (total=%s%s)%s"
                                 (cond
                                  ((> indicator-total 1e6) (format "%.1fM" (/ indicator-total 1000000.0)))
                                  ((> indicator-total 1e3) (format "%.1fK" (/ indicator-total 1000.0)))
                                  (t indicator-total))
                                 (if indicator-done "" "+")
                                 (if indicator-active (propertize ":" 'face
                                                                  `(:foreground ,(face-attribute
                                                                                  'error :foreground))) ":"))))))
    (lambda (action)
      (pcase action
        ((pred stringp)
         (pcase-let ((`(,re . ,hl) (funcall affe-regexp-compiler action 'emacs 'ignore-case)))
           (setq re (seq-filter #'consult--valid-regexp-p re))
           (unless (or (not re) (equal re regexps))
             (setq regexps re
                   highlight hl)
             (affe--send proc `(search ,affe-count ,@re)))))
        ('destroy
         (affe--send proc 'exit)
         (funcall async 'destroy)
         (delete-overlay indicator))
        ('setup
         (setq indicator (make-overlay (- (minibuffer-prompt-end) 2)
                                       (- (minibuffer-prompt-end) 1)))
         (funcall async 'setup)
         (call-process
          (file-truename
           (expand-file-name invocation-name
                             invocation-directory))
          nil nil nil "-Q"
          (concat "--daemon=" name)
          ;; Invoking Emacs on Mac requires specifying the directory (See gh #3 and bug#48579).
          (concat "--chdir=" default-directory)
          "-l" backend)
         (setq proc (affe--connect name callback))
         (affe--send proc `(start ,regexp ,@cmd))
         (affe--send proc `(search ,affe-count)))
        (_ (funcall async action))))))

(defun affe--command (cmd paths)
  "Build command line argument list from CMD string and PATHS."
  (setq cmd (split-string-and-unquote cmd))
  (if (member "." cmd)
      (seq-mapcat (lambda (x) (if (equal x ".") paths (list x))) cmd)
    (append cmd paths)))

;;;###autoload
(defun affe-grep (&optional dir initial)
  "Fuzzy grep in DIR with optional INITIAL input."
  (interactive "P")
  (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fuzzy grep" dir))
               (default-directory dir))
    (consult--read
     (thread-first (consult--async-sink)
       (consult--async-refresh-timer 0.05)
       (consult--grep-format nil)
       (affe--async
        (affe--command affe-grep-command paths)
        "\\`[^\0]+\0[^\0:]+[\0:]\\(.*\\)\\'")
       (consult--async-split #'consult--split-nil))
     :prompt prompt
     :sort nil
     :require-match t
     :initial initial
     :history '(:input affe--grep-history)
     :category 'consult-grep
     :add-history (thing-at-point 'symbol)
     :lookup #'consult--lookup-member
     :group #'consult--prefix-group
     :state (consult--grep-state))))

;;;###autoload
(defun affe-find (&optional dir initial)
  "Fuzzy find in DIR with optional INITIAL input."
  (interactive "P")
  (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fuzzy find" dir))
               (default-directory dir))
    (consult--read
     (thread-first (consult--async-sink)
       (consult--async-refresh-timer 0.05)
       (consult--async-map (lambda (x) (string-remove-prefix "./" x)))
       (affe--async (affe--command affe-find-command paths))
       (consult--async-split #'consult--split-nil))
     :prompt prompt
     :sort nil
     :require-match t
     :history '(:input affe--find-history)
     :initial initial
     :category 'file
     :add-history (thing-at-point 'filename)
     :state (lambda (action cand)
              (when (and cand (eq action 'return))
                (find-file cand))))))

(provide 'affe)
;;; affe.el ends here
