;;; affe-backend.el --- AFFE Backend -*- lexical-binding: t -*-

;; Author: Daniel Mendler
;; Maintainer: Daniel Mendler
;; Created: 2021
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/affe

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
                         (process-send-string client (concat (and (= count 0) "-affe-first\n")
                                                             "-affe-match " cand "\n"))
                         (when (>= (setq count (1+ count)) limit)
                           (throw 'done nil))
                         nil)))
    (process-send-string client (if (= count 0) "-affe-failed\n" "-affe-finished\n"))
    nil))

(provide 'affe-backend)
;;; affe-backend.el ends here
