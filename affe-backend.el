;;; affe-backend.el --- AFFE Backend -*- lexical-binding: t -*-

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

;; AFFE Backend

;;; Code:

(require 'server)

(setq gc-cons-threshold 67108864
      gc-cons-percentage 0.5)

(defvar affe-backend--head (list ""))
(defvar affe-backend--tail affe-backend--head)
(defvar affe-backend--last nil)

(defun affe-backend--process-filter (_ out)
  "Process filter function receiving output string OUT."
  (setq affe-backend--tail (last (setcdr affe-backend--tail
                                    (split-string out "\n" 'omit-nulls)))))

(defun affe-backend-start (&rest cmd)
  "Start backend CMD."
  (run-at-time 0 nil
               #'make-process
               :name (car cmd)
               :noquery t
               :command cmd
               :connection-type 'pipe
               :stderr "*stderr*"
               :filter #'affe-backend--process-filter)
  nil)

(defun affe-backend-filter (limit &rest regexps)
  "Filter backend lines with REGEXPS returning up to LIMIT matching lines."
  (let ((completion-regexp-list regexps)
        (completion-ignore-case t)
        (client (car server-clients))
        (count 0))
    (catch 'done
      (all-completions "" (cdr affe-backend--head)
                       (lambda (cand)
                         (process-send-string client (concat (and (= count 0) "-affe-flush\n")
                                                             "-affe-match " cand "\n-affe-refresh\n"))
                         (when (>= (setq count (1+ count)) limit)
                           (throw 'done nil))
                         nil)))
    (when (= count 0)
      (process-send-string client "-affe-flush\n")
      (process-send-string client "-affe-refresh\n"))
    nil))

(provide 'affe-backend)
;;; affe-backend.el ends here
