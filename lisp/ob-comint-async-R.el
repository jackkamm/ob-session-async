;;; ob-comint-async-R.el --- Async Babel Interaction with R sessions -*- lexical-binding: t -*-

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
(require 'ob-comint-async)

(defun ob-comint-async-org-babel-execute:R (orig-fun body params)
  "TODO"
  (let ((async (assq :async params))
        (session (assq :session params)))
    (if (or (not async)
            (equal (cdr session) "none"))
        (funcall orig-fun body params)
      (advice-add 'org-babel-R-evaluate-session
                  :override 'org-babel-R-evaluate-session-async)
      (let ((result (funcall orig-fun body params)))
        (advice-remove 'org-babel-R-evaluate-session
                       'org-babel-R-evaluate-session-async)
        result))))

(advice-add 'org-babel-execute:R :around 'ob-comint-async-org-babel-execute:R)

(defconst org-babel-R-async-indicator "'org_babel_R_async_%s_%s'")
(defconst org-babel-R-async-indicator-output
  "^\\[1\\] \"org_babel_R_async_\\(.+\\)_\\(.+\\)\"$")

(defun org-babel-R-evaluate-session-async
    (session body result-type result-params column-names-p row-names-p)
  "Asynchronously evaluate BODY in SESSION.
Returns a placeholder string for insertion, to later be replaced
by `org-babel-comint-async-filter'."
  (org-babel-comint-async-register session (current-buffer)
				   org-babel-R-async-indicator-output
				   'org-babel-R-async-output-callback
				   'org-babel-R-async-value-callback)
  (cl-case result-type
    (value
     (let ((tmp-file (org-babel-temp-file "R-")))
       (with-temp-buffer
       (insert
	(org-babel-chomp body))
       (let ((ess-local-process-name
	      (process-name (get-buffer-process session)))
	     (ess-eval-visibly-p nil))
	 (ess-eval-buffer nil)))
       (with-temp-buffer
	 (insert
	  (mapconcat 'org-babel-chomp
		     (list (org-babel-R-write-last-value-command row-names-p
								 column-names-p
								 tmp-file)
			   (format org-babel-R-async-indicator
				   "file" tmp-file))
		     "\n"))
	 (let ((ess-local-process-name
		(process-name (get-buffer-process session)))
	       (ess-eval-visibly-p nil))
	   (ess-eval-buffer nil)))
       tmp-file))
    (output
     (let ((uuid (md5 (number-to-string (random 100000000)))))
       (org-babel-comint-delete-dangling-and-eval
	   session
	 (insert (mapconcat 'org-babel-chomp
			    (list (format org-babel-R-async-indicator
					  "start" uuid)
				  body
				  (format org-babel-R-async-indicator
					  "end" uuid))
			    "\n"))
	 (inferior-ess-send-input))
       uuid))))

(defun org-babel-R-async-output-callback (output)
  "Callback for async output results.
Assigned locally to `org-babel-comint-async-chunk-callback' in R
comint buffers used for asynchronous Babel evaluation."
  (mapconcat
   'org-babel-chomp
   (cdr (butlast (mapcar (lambda (line) (string-remove-prefix "\n" line))
			 (org-babel-R-clean-session-output output))))
   "\n"))

(defun org-babel-R-async-value-callback (params tmp-file)
  "Callback for async value results.
Assigned locally to `org-babel-comint-async-file-callback' in R
comint buffers used for asynchronous Babel evaluation."
  (org-babel-R-process-value-result
   (org-babel-r-value-from-tmp-file
    (assq :result-params params) tmp-file)
   ;; TODO this is not exactly the same as colnames-p above...
   (org-babel-R-get-colnames-p params)))


;; helper functions

(defun org-babel-R-value-from-tmp-file (result-params tmp-file)
  "Insert result from TMP-FILE with RESULT-PARAMS."
  (org-babel-result-cond result-params
	  (with-temp-buffer
	    (insert-file-contents tmp-file)
	    (org-babel-chomp (buffer-string) "\n"))
	  (org-babel-import-elisp-from-file tmp-file '(16))))

(defun org-babel-R-clean-session-output (output)
  "Remove extra prompts and empty lines from OUTPUT."
  (delq nil
	(mapcar
	 (lambda (line) (when (> (length line) 0) line))
	 (mapcar
	  (lambda (line) ;; cleanup extra prompts left in output
	    (if (string-match
		 "^\\([>+.]\\([ ][>.+]\\)*[ ]\\)"
		 (car (split-string line "\n")))
		(substring line (match-end 1))
	      line))
	  output))))

(defun org-babel-R-write-last-value-command (row-names-p column-names-p tmp-file)
  "Generate R command to output last value to TMP-FILE."
  (format org-babel-R-write-object-command
	  (if row-names-p "TRUE" "FALSE")
	  (if column-names-p
	      (if row-names-p "NA" "TRUE")
	    "FALSE")
	  ".Last.value" (org-babel-process-file-name tmp-file 'noquote)))

(defun org-babel-R-get-colnames-p (params)
  "Determine whether to use column names from PARAMS of R Babel block."
  (let* ((graphics-file (and (member "graphics" (assq :result-params params))
			     (org-babel-graphical-output-file params)))
	 (colnames-p (unless graphics-file (cdr (assq :colnames params)))))
    (or (equal "yes" colnames-p)
	(org-babel-pick-name
	 (cdr (assq :colname-names params)) colnames-p))))

(provide 'ob-comint-async-R)

;;; ob-comint-async-R.el ends here
