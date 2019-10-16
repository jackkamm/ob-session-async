;;; ob-session-async-ruby.el --- Async Babel Interaction with Ruby sessions -*- lexical-binding: t -*-

;; Copyright (C) 2019

;; Author:  <jackkamm@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Support for evaluating Ruby code asynchronously in org-babel sessions.

;;; Code:

(require 'ob-ruby)
(require 'ob-session-async)
(require 'subr-x)

(defun ob-session-async-org-babel-execute:ruby (orig-fun body params)
  "Advice around `org-babel-execute:ruby' to enable asynchronous evaluation."
  (let ((async (assq :async params))
         (session (assq :session params)))
    (if (or (not async)
          (equal (cdr async) "no")
          (equal (cdr session) "none"))
      (funcall orig-fun body params)
      (advice-add 'org-babel-ruby-evaluate
        :override 'ob-session-async-org-babel-ruby-evaluate-session)
      (let ((result (funcall orig-fun body params)))
        (advice-remove 'org-babel-ruby-evaluate 'ob-session-async-org-babel-ruby-evaluate-session)
        result))))

(advice-add 'org-babel-execute:ruby :around 'ob-session-async-org-babel-execute:ruby)

(defconst ob-session-async-ruby-indicator "puts 'ob_comint_async_ruby_%s_%s'")

(defun ob-session-async-ruby-chunk-callback (string)
  (let* ((prefix-regexp "^=> nil\nirb([A-Za-z]+)[0-9:]+> ")
          (suffix-regexp "\n=> true\nirb([A-Za-z]+)[0-9:]+>$")
          (new-string (replace-regexp-in-string suffix-regexp ""
                        (replace-regexp-in-string prefix-regexp "" string))))
    (org-babel-chomp new-string)))

(defun ob-session-async-org-babel-ruby-evaluate-session (buffer body &optional result-type result-params)
  "Asynchronously evaluate BODY in SESSION.
Returns a placeholder string for insertion, to later be replaced
by `ob-session-async-filter'."
  (ob-session-async-register
    buffer (current-buffer)
    "ob_comint_async_ruby_\\(.+\\)_\\(.+\\)"
    'ob-session-async-ruby-chunk-callback
    'ob-session-async-ruby-value-callback)
  ;; comint session evaluation
  (pcase result-type
    (`output
      (let* ((uuid (md5 (number-to-string (random 100000000)))))
        (with-current-buffer buffer
          (mapc
            (lambda (line)
              (insert (org-babel-chomp line)) (comint-send-input nil t))
            (list
              (format ob-session-async-ruby-indicator "start" uuid)
              "conf.echo=false;_org_prompt_mode=conf.prompt_mode;conf.prompt_mode=:NULL"
              body
              "conf.prompt_mode=_org_prompt_mode;conf.echo=true"
              (format ob-session-async-ruby-indicator "end" uuid)))
          (comint-send-input))
        uuid))
    (`value
      (let* ((tmp-file (org-babel-temp-file "ruby-"))
              (ppp (or (member "code" result-params)
                     (member "pp" result-params))))
        (with-current-buffer buffer
          ;; (buffer org-babel-ruby-eoe-indicator t body)
          (when ppp (insert "\nrequire 'pp';") (comint-send-input nil t))
          (insert "\n")
          (mapc
            (lambda (line)
              (insert (org-babel-chomp line)) (comint-send-input nil t))
            (append
              (list body)
              (if (not ppp)
                (list (format org-babel-ruby-f-write
                        (org-babel-process-file-name tmp-file 'noquote)))
                (list
                  "results=_" "require 'pp'" "orig_out = $stdout"
                  (format org-babel-ruby-pp-f-write
                    (org-babel-process-file-name tmp-file 'noquote))))
              (list (format ob-session-async-ruby-indicator "file" tmp-file))))
          (comint-send-input nil t))
        tmp-file))))

(defun ob-session-async-ruby-value-callback (params tmp-file)
  "Callback for async value results.
Assigned locally to `ob-session-async-file-callback' in Ruby
comint buffers used for asynchronous Babel evaluation."
  (with-temp-buffer
    (insert-file-contents tmp-file)
    (buffer-string)))

(provide 'ob-session-async-ruby)

;;; ob-session-async-ruby.el ends here
