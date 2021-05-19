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

(setq gc-cons-threshold 67108864
      gc-cons-percentage 0.5)

(defvar affe-backend--head (list ""))
(defvar affe-backend--tail affe-backend--head)
(defvar affe-backend--last nil)

(defun affe-backend--process-filter (_ out)
  "Process filter function receiving output string OUT."
  (setq affe-backend--tail (last (setcdr affe-backend--tail
                                    (split-string out "\n" 'omit-nulls)))))

(defun affe-backend-start (cmd)
  "Start backend CMD."
  (setq cmd (split-string-and-unquote cmd))
  (make-process
   :name (car cmd)
   :noquery t
   :command cmd
   :connection-type 'pipe
   :stderr "*stderr*"
   :filter #'affe-backend--process-filter)
  nil)

(defun affe-backend-filter (&rest regexps)
  "Filter backend output with REGEXPS."
  (let ((completion-regexp-list regexps)
        (completion-ignore-case t)
        (count 0)
        (result nil))
    (catch 'done
      (all-completions "" (cdr affe-backend--head)
                       (lambda (cand)
                         (push cand result)
                         (when (>= (setq count (1+ count)) 20)
                           (throw 'done nil))
                         nil)))
    (nreverse result)))

(provide 'affe-backend)
;;; affe-backend.el ends here
