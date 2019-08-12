;;; ob-session-async-R.el --- Async Babel Interaction with R sessions -*- lexical-binding: t -*-

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

;; TODO

;;; Code:

(require 'ob-R)
(require 'ob-session-async)
(require 'subr-x)

(defun ob-session-async-org-babel-execute:R (orig-fun body params)
  "TODO"
  (let ((async (assq :async params))
        (session (assq :session params)))
    (if (or (not async)
            (equal (cdr async) "no")
            (equal (cdr session) "none"))
        (funcall orig-fun body params)
      (let (ess-eval-visibly)
        (advice-add 'org-babel-R-evaluate-session
                    :override 'ob-session-async-org-babel-R-evaluate-session)
        (let ((result (funcall orig-fun body params)))
          (advice-remove 'org-babel-R-evaluate-session
                         'ob-session-async-org-babel-R-evaluate-session)
          result)))))

(advice-add 'org-babel-execute:R :around 'ob-session-async-org-babel-execute:R)

(defconst ob-session-async-R-indicator "'ob_comint_async_R_%s_%s'")

(defun ob-session-async-org-babel-R-evaluate-session
    (session body result-type result-params column-names-p row-names-p)
  "Asynchronously evaluate BODY in SESSION.
Returns a placeholder string for insertion, to later be replaced
by `ob-session-async-filter'."
  (ob-session-async-register
   session (current-buffer)
   "^\\(?:[>.+] \\)*\\[1\\] \"ob_comint_async_R_\\(.+\\)_\\(.+\\)\"$"
   'org-babel-chomp
   'ob-session-async-R-value-callback)
  (cl-case result-type
    (value
     (let ((tmp-file (org-babel-temp-file "R-")))
       (with-temp-buffer
         (insert
          (org-babel-chomp body))
         (let ((ess-local-process-name
                (process-name (get-buffer-process session))))
           (ess-eval-buffer nil)))
       (with-temp-buffer
	 (insert
	  (mapconcat
           'org-babel-chomp
           (list (format org-babel-R-write-object-command
                         (if row-names-p "TRUE" "FALSE")
                         (if column-names-p
                             (if row-names-p "NA" "TRUE")
                           "FALSE")
                         ".Last.value"
                         (org-babel-process-file-name tmp-file 'noquote))
                 (format ob-session-async-R-indicator
                         "file" tmp-file))
           "\n"))
	 (let ((ess-local-process-name
		(process-name (get-buffer-process session))))
	   (ess-eval-buffer nil)))
       tmp-file))
    (output
     (let ((uuid (md5 (number-to-string (random 100000000))))
           (ess-local-process-name
            (process-name (get-buffer-process session))))
       (with-temp-buffer
         (insert (format ob-session-async-R-indicator
					  "start" uuid))
         (insert "\n")
         (insert body)
         (insert "\n")
         (insert (format ob-session-async-R-indicator
					  "end" uuid))
         (ess-eval-buffer nil))
       uuid))))

(defun ob-session-async-R-value-callback (params tmp-file)
  "Callback for async value results.
Assigned locally to `ob-session-async-file-callback' in R
comint buffers used for asynchronous Babel evaluation."
  (let* ((graphics-file (and (member "graphics" (assq :result-params params))
			     (org-babel-graphical-output-file params)))
	 (colnames-p (unless graphics-file (cdr (assq :colnames params)))))
    (org-babel-R-process-value-result
     (org-babel-result-cond (assq :result-params params)
       (with-temp-buffer
         (insert-file-contents tmp-file)
         (org-babel-chomp (buffer-string) "\n"))
       (org-babel-import-elisp-from-file tmp-file '(16)))
     (or (equal "yes" colnames-p)
	 (org-babel-pick-name
	  (cdr (assq :colname-names params)) colnames-p)))))

(provide 'ob-session-async-R)

;;; ob-session-async-R.el ends here
