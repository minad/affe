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

;; Increase GC limit in the background process
(setq gc-cons-threshold 67108864
      gc-cons-percentage 0.5)

(defvar affe-backend--head (list nil))
(defvar affe-backend--tail affe-backend--head)
(defvar affe-backend--total 0)
(defvar affe-backend--done nil)
(defvar affe-backend--searching nil)

(defun affe-backend--status ()
  (when (car server-clients)
    (ignore-errors (process-send-string (car server-clients)
                                        (format "-affe-status %S\n"
                                                (list affe-backend--total
                                                      affe-backend--done
                                                      affe-backend--searching))))))

(defun affe-backend--process-filter (_ out)
  "Process filter function receiving output string OUT."
  (setq out (split-string out "\n" 'omit-nulls)
        affe-backend--total (+ affe-backend--total (length out))
        affe-backend--tail (last (setcdr affe-backend--tail out))))

(defun affe-backend--process-sentinel (&rest _)
  (setq affe-backend--done t))

(defun affe-backend-start (&rest cmd)
  "Start backend CMD."
  (make-process
   :name (car cmd)
   :noquery t
   :command cmd
   :connection-type 'pipe
   :stderr "*stderr*"
   :sentinel #'affe-backend--process-sentinel
   :filter #'affe-backend--process-filter)
  (while t
    (affe-backend--status)
    (sleep-for 0.5)))

(defun affe-backend-filter (limit &rest regexps)
  "Filter backend lines with REGEXPS returning up to LIMIT matching lines."
  (let ((completion-regexp-list regexps)
        (completion-ignore-case t)
        (client (car server-clients))
        (affe-backend--searching t)
        (count 0))
    (affe-backend--status)
    (catch 'affe--done
      (all-completions "" (cdr affe-backend--head)
                       (lambda (cand)
                         (affe-backend--status)
                         (process-send-string client (concat (and (= count 0) "-affe-flush\n")
                                                             "-affe-match " cand "\n"))
                         (when (>= (setq count (1+ count)) limit)
                           (throw 'affe--done nil))
                         nil)))
    (when (= count 0)
      (process-send-string client "-affe-flush\n")))
  (affe-backend--status)
  nil)

(provide 'affe-backend)
;;; affe-backend.el ends here
