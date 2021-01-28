;;; consult-fuzzy.el --- Fuzzy finding using Consult -*- lexical-binding: t -*-

;; Author: Daniel Mendler
;; Maintainer: Daniel Mendler
;; Created: 2021
;; License: GPL-3.0-or-later
;; Version: 0.2
;; Package-Requires: ((emacs "26.1") (consult "0.2"))
;; Homepage: https://github.com/minad/consult-fuzzy

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

;; Fuzzy finder

;;; Code:

(require 'consult)

(defgroup consult-fuzzy nil
  "Consult-Fuzzying `completing-read'."
  :group 'convenience
  :prefix "consult-fuzzy-")

;; Alternatives:
;; "rg --line-buffered -m 100 -i \"$0\""
;; "grep --line-buffered -m 100 -i -E \"$0\""
;; "fzy -e \"$0\" | head -n 100"
(defcustom consult-fuzzy-filter-command "fzf -f \"$0\" | head -n 100"
  "Filtering command."
  :type 'string)

(defcustom consult-fuzzy-find-command "find -not '(' -wholename '*/.*' -prune ')' -type f"
  "Find command."
  :type 'string)

(defcustom consult-fuzzy-grep-command "rg --line-buffered -n --color=never -v '^$' ."
  "Grep command."
  :type 'string)

(defvar consult-fuzzy--grep-history nil)
(defvar consult-fuzzy--find-history nil)

(defconst consult-fuzzy--directory
  (or (and load-file-name
           (file-name-directory load-file-name))
      default-directory))

(defun consult-fuzzy--executable ()
  "Return executable name."
  (concat consult-fuzzy--directory "consult-fuzzy.bin"))

(defun consult-fuzzy--build ()
  "Build executable."
  (unless (file-exists-p (consult-fuzzy--executable))
    (let ((default-directory consult-fuzzy--directory)
          (inhibit-message t)
          (buf "*consult-fuzzy-build*"))
      (when (/= 0 (shell-command
                   (format "cc -std=c11 -D_POSIX_C_SOURCE=1 -O2 -Wall -Wextra -o '%s' consult-fuzzy.c"
                           (consult-fuzzy--executable))
                   buf buf))
        (error "Compiling consult-fuzzy failed, see %s" buf)))))

(defun consult-fuzzy--async (async cmd)
  "Create process source async function.

ASYNC is the async function which receives the candidates.
CMD is the command argument list."
  (let ((proc) (flush) (rest "") (last-input ""))
    (lambda (action)
      (pcase action
        ((pred stringp)
         (unless (or (string= action "") (string= action last-input))
           (process-send-string proc (concat action "\n"))
           (setq last-input action)))
        ('destroy
         (ignore-errors (delete-process proc))
         (funcall async 'destroy))
        ('setup
         (funcall async 'setup)
         (consult--async-log "consult-fuzzy--async started %S\n" cmd)
         (setq
          proc
          (make-process
           :name (car cmd)
           :stderr consult--async-log
           :noquery t
           :command cmd
           :filter
           (lambda (_ out)
             (when-let (pos (seq-position out 3)) ;; ETX
               (when flush
                 (setq flush nil)
                 (funcall async 'flush))
               (setq out (concat (substring out 0 pos) (substring out (1+ pos)))))
             (when-let (pos (seq-position out 2)) ;; STX
               (setq flush t rest "" out (substring out (1+ pos))))
             (let ((lines (split-string out "\n")))
               (if (not (cdr lines))
                   (setq rest (concat rest (car lines)))
                 (setcar lines (concat rest (car lines)))
                 (setq rest (car (last lines)))
                 (when flush
                   (setq flush nil)
                   (funcall async 'flush))
                 (funcall async (nbutlast lines)))))
           :sentinel
           (lambda (_ event)
             (consult--async-log "consult-fuzzy--async sentinel: %s\n" event)
             (when (and (string-prefix-p "finished" event) (not (string= rest "")))
               (when flush
                 (setq flush nil)
                 (funcall async 'flush))
               (funcall async (list rest)))))))
        (_ (funcall async action))))))

(defun consult--fuzzy-command (prompt cmd history dir initial)
  "Run fuzzy CMD with PROMPT, HISTORY and INITIAL input in DIR."
  (consult-fuzzy--build)
  (let* ((prompt-dir (consult--directory-prompt prompt dir))
         (default-directory (cdr prompt-dir)))
    (consult--read
     (car prompt-dir)
     (thread-first (consult--async-sink)
       (consult--async-refresh-immediate)
       (consult--async-map (lambda (x) (string-remove-prefix "./" x)))
       (consult-fuzzy--async (list (consult-fuzzy--executable) cmd
                                    consult-fuzzy-filter-command))
       (consult--async-split))
     :sort nil
     :require-match t
     :initial (concat consult-async-default-split initial)
     :add-history (concat consult-async-default-split (thing-at-point 'filename))
     :category 'file
     :history (list :input history))))

(defun consult-fuzzy-find (&optional dir initial)
  "Fuzzy find in directory DIR with INITIAL input."
  (interactive "P")
  (find-file (consult--fuzzy-command
              "Fuzzy find"
              consult-fuzzy-find-command
              'consult-fuzzy--find-history dir initial)))

(defun consult-fuzzy-grep (&optional dir initial)
  "Fuzzy grep in directory DIR with INITIAL inut."
  (interactive "P")
  (consult-fuzzy--goto (consult--fuzzy-command
                        "Fuzzy grep"
                        consult-fuzzy-grep-command
                        'consult-fuzzy--grep-history dir initial)))

(defun consult-fuzzy--goto (location)
  "Goto grep LOCATION."
  (with-temp-buffer
    (insert location "\n")
    (grep-mode)
    (let ((display-buffer-overriding-action '(display-buffer-same-window))
          (inhibit-message t))
      (next-error 0 t))))

(provide 'consult-fuzzy)
;;; consult-fuzzy.el ends here
