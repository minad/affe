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

(require 'server)

(defvar affe-backend--head (list nil))
(defvar affe-backend--tail affe-backend--head)
(defvar affe-backend--total 0)
(defvar affe-backend--done nil)
(defvar affe-backend--searching nil)
(defvar affe-backend--server-rest "")

(defun affe-backend--status (client)
  "Send status to the CLIENT."
  (affe-backend--send client
              `(status ,affe-backend--total
                       ,affe-backend--done
                       ,affe-backend--searching)))

(defun affe-backend--process-filter (_ out)
  "Process filter function receiving output string OUT."
  (setq out (split-string out "\n" 'omit-nulls)
        affe-backend--total (+ affe-backend--total (length out))
        affe-backend--tail (last (setcdr affe-backend--tail out))))

(defun affe-backend--server-filter (proc out)
  (let ((lines (split-string out "\n"))
        (filter nil))
    (if (not (cdr lines))
        (setq affe-backend--server-rest (concat affe-backend--server-rest (car lines)))
      (setcar lines (concat affe-backend--server-rest (car lines)))
      (setq affe-backend--server-rest (car (last lines)))
      (dolist (line (nbutlast lines))
        (pcase (read line)
          ('exit (kill-emacs))
          (`(filter . ,f) (setq filter f))
          (`(start . ,cmd)
           (run-at-time 0.5 0.5 #'affe-backend--status proc)
           (affe-backend--process-start cmd)))))
    (when filter
      (affe-backend--filter proc (car filter) (cdr filter)))))

(defun affe-backend--process-start (cmd)
  "Start backend CMD."
  (make-process
   :name (car cmd)
   :noquery t
   :command cmd
   :connection-type 'pipe
   :stderr "*stderr*"
   :sentinel (lambda (&rest _) (setq affe-backend--done t))
   :filter #'affe-backend--process-filter))

(defun affe-backend--send (proc expr)
  "Send EXPR to PROC."
  (process-send-string
   proc
   (let ((print-escape-newlines t))
     (concat (prin1-to-string expr) "\n"))))

(defun affe-backend--filter (client limit regexps)
  "Filter lines with REGEXPS returning up to LIMIT matching lines to the CLIENT."
  (let ((completion-regexp-list regexps)
        (completion-ignore-case t)
        (affe-backend--searching t)
        (count 0))
    (affe-backend--status client)
    (catch 'affe--done
      (all-completions "" (cdr affe-backend--head)
                       (lambda (cand)
                         (affe-backend--status client)
                         (when (= count 0)
                           (affe-backend--send client 'flush))
                         (affe-backend--send client `(match . ,cand))
                         (when (>= (setq count (1+ count)) limit)
                           (throw 'affe--done nil))
                         nil)))
    (when (= count 0)
      (affe-backend--send client 'flush)))
  (affe-backend--status client))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 67108864
                  gc-cons-percentage 0.5)
            (set-process-coding-system server-process 'no-conversion 'no-conversion)
            (set-process-filter server-process #'affe-backend--server-filter)))

(provide 'affe-backend)
;;; affe-backend.el ends here
