;;; affe-backend.el --- Affe backend -*- lexical-binding: t -*-

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

;; Affe backend

;;; Code:

(setq gc-cons-threshold 67108864
      gc-cons-percentage 0.5)

(require 'server)

(defvar affe-backend--search-head (list nil))
(defvar affe-backend--search-tail affe-backend--search-head)
(defvar affe-backend--search-found 0)
(defvar affe-backend--search-limit 0)
(defvar affe-backend--search-regexps nil)

(defvar affe-backend--producer-head (list nil))
(defvar affe-backend--producer-tail affe-backend--producer-head)
(defvar affe-backend--producer-total 0)
(defvar affe-backend--producer-done nil)
(defvar affe-backend--producer-rest "")

(defvar affe-backend--client-rest "")
(defvar affe-backend--client nil)

(defun affe-backend--send (expr)
  "Send EXPR."
  (process-send-string
   affe-backend--client
   (let ((print-escape-newlines t))
     (concat (prin1-to-string expr) "\n"))))

(defun affe-backend--producer-filter (_ out)
  (let ((lines (split-string out "\n")))
    (if (not (cdr lines))
        (setq affe-backend--producer-rest (concat affe-backend--producer-rest (car lines)))
      (setcar lines (concat affe-backend--producer-rest (car lines)))
      (let* ((len (length lines))
             (last (nthcdr (- len 2) lines)))
        (setcdr affe-backend--producer-tail lines)
        (setq affe-backend--producer-rest (cadr last)
              affe-backend--producer-total (+ affe-backend--producer-total len -1)
              affe-backend--producer-tail last)
        (setcdr last nil)))))

(defun affe-backend--producer-sentinel (&rest _)
  (setq affe-backend--producer-done t)
  (unless (equal affe-backend--producer-rest "")
    (setq affe-backend--producer-total (1+ affe-backend--producer-total)
          affe-backend--producer-tail (setcdr affe-backend--producer-tail (list affe-backend--producer-rest)))))

(defun affe-backend--producer-start (cmd)
  "Start backend CMD."
  (make-process
   :name (car cmd)
   :noquery t
   :command cmd
   :connection-type 'pipe
   :stderr "*stderr*"
   :sentinel #'affe-backend--producer-sentinel
   :filter #'affe-backend--producer-filter))

(defun affe-backend--server-filter (client out)
  "Server filter function receiving CLIENT and OUT string."
  (let ((lines (split-string out "\n")))
    (if (not (cdr lines))
        (setq affe-backend--client-rest (concat affe-backend--client-rest (car lines)))
      (setcar lines (concat affe-backend--client-rest (car lines)))
      (let ((last (last lines 2)))
        (setq affe-backend--client-rest (cadr last))
        (setcdr last nil))
      (dolist (line lines)
        (pcase (read line)
          ('exit (kill-emacs))
          (`(search ,limit . ,regexps)
           (setcdr affe-backend--search-tail (cdr affe-backend--producer-head))
           (setq affe-backend--producer-head affe-backend--search-head
                 affe-backend--search-head (list nil)
                 affe-backend--search-tail affe-backend--search-head
                 affe-backend--search-limit limit
                 affe-backend--search-found 0
                 affe-backend--search-regexps regexps))
          (`(start . ,cmd)
           (setq affe-backend--client client)
           (run-at-time 0.5 0.5 #'affe-backend--producer-refresh)
           (run-at-time 0.1 0.1 #'affe-backend--search-refresh)
           (affe-backend--producer-start cmd))))
      (when (/= 0 affe-backend--search-limit)
        (affe-backend--search)
        (run-at-time 0.5 nil #'affe-backend--flush)))))

(defun affe-backend--flush ()
  "Send a flush if no matching strings are found."
  (when (= 0 affe-backend--search-found)
    (affe-backend--send 'flush)))

(defun affe-backend--producer-refresh ()
  "Refresh backend, continue search and send status."
  (affe-backend--send
   `(producer ,affe-backend--producer-total
              ,affe-backend--producer-done)))

(defun affe-backend--search-refresh ()
  (when (/= 0 affe-backend--search-limit)
    (affe-backend--search)))

(defun affe-backend--search-status ()
  "Send status to the CLIENT."
  (affe-backend--send `(search ,(/= 0 affe-backend--search-limit))))

(defun affe-backend--search-match-found (match)
  "Called when matching string MATCH has been found."
  (affe-backend--search-status)
  (affe-backend--flush)
  (affe-backend--send `(match ,match))
  (when (>= (setq affe-backend--search-found (1+ affe-backend--search-found))
            affe-backend--search-limit)
    (throw 'affe-backend--search-done nil))
  nil)

(defun affe-backend--search ()
  "Search and send matching lines to client."
  (affe-backend--search-status)
  (let ((completion-regexp-list affe-backend--search-regexps)
        (completion-ignore-case t)
        (head (cdr affe-backend--producer-head)))
    (setcdr affe-backend--search-tail head)
    (setq affe-backend--search-tail affe-backend--producer-tail
          affe-backend--producer-head (list nil)
          affe-backend--producer-tail affe-backend--producer-head)
    (catch 'affe-backend--search-done
      (all-completions "" head #'affe-backend--search-match-found)))
  (when (or (>= affe-backend--search-found affe-backend--search-limit)
            (and affe-backend--producer-done
                 (not (cdr affe-backend--producer-head))))
    (setq affe-backend--search-limit 0))
  (when (= 0 affe-backend--search-limit)
    (affe-backend--flush))
  (affe-backend--search-status))

(defun affe-backend--setup ()
  (set-process-coding-system server-process 'no-conversion 'no-conversion)
  (set-process-filter server-process #'affe-backend--server-filter))

(add-hook 'emacs-startup-hook #'affe-backend--setup)

(provide 'affe-backend)
;;; affe-backend.el ends here
