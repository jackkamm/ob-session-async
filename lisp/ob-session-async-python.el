;;; ob-session-async-python.el --- Async Babel Interaction with R sessions -*- lexical-binding: t -*-

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

;; Support for evaluating python code asynchronously in org-babel sessions.

;;; Code:

(require 'ob-python)
(require 'ob-session-async)
(require 'subr-x)

(defun ob-session-async-org-babel-execute:python (orig-fun body params)
  "Advice around `org-babel-execute:python' to enable asynchronous evaluation."
  (let ((async (assq :async params))
        (session (assq :session params)))
    (if (or (not async)
            (equal (cdr async) "no")
            (equal (cdr session) "none"))
        (funcall orig-fun body params)
      (advice-add 'org-babel-python-evaluate-session
                  :override 'ob-session-async-org-babel-python-evaluate-session)
      (let ((result (funcall orig-fun body params)))
        (advice-remove 'org-babel-python-evaluate-session
                       'ob-session-async-org-babel-python-evaluate-session)
        result))))

(advice-add 'org-babel-execute:python :around 'ob-session-async-org-babel-execute:python)

(defconst ob-session-async-python-indicator "print ('ob_comint_async_python_%s_%s')")

(defun ob-session-async-python-value-callback (params tmp-file)
  (org-babel-eval-read-file tmp-file))

(defun ob-session-async-org-babel-python-evaluate-session
    (session body &optional result-type result-params)
  "Asynchronously evaluate BODY in SESSION.
Returns a placeholder string for insertion, to later be replaced
by `ob-session-async-filter'."
  (ob-session-async-register
   session (current-buffer)
   "ob_comint_async_python_\\(.+\\)_\\(.+\\)"
   'org-babel-chomp 'ob-session-async-python-value-callback)
  (let ((python-shell-buffer-name (org-babel-python-without-earmuffs session)))
    (pcase result-type
      (`output
       (let ((uuid (md5 (number-to-string (random 100000000)))))
         (with-temp-buffer
           (insert (format ob-session-async-python-indicator "start" uuid))
           (insert "\n")
           (insert body)
           (insert "\n")
           (insert (format ob-session-async-python-indicator "end" uuid))
           (python-shell-send-buffer))
         uuid))
      (`value
       ;; TODO: require dependancy of Org 9.4 (not yet released as of 2020-02-15)
       (let ((tmp-results-file (org-babel-temp-file "python-"))
             (tmp-src-file (org-babel-temp-file "python-")))
         (with-temp-file tmp-src-file (insert body))
         (with-temp-buffer
           (insert (format org-babel-python--eval-ast tmp-src-file))
           (insert (format (if (member "pp" result-params)
                               "
import pprint
with open('%s', 'w') as f:
    f.write(pprint.pformat(__org_babel_python_final))"
                             "
with open('%s', 'w') as f:
    f.write(str(__org_babel_python_final))")
                           (org-babel-process-file-name
                            tmp-results-file 'noquote)))
           (insert "\n")
           (insert (format ob-session-async-python-indicator "file" tmp-results-file))
           (python-shell-send-buffer))
         tmp-results-file)))))

(provide 'ob-session-async-python)

;;; ob-session-async-python.el ends here
