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
                  :override 'ob-comint-async-org-babel-R-evaluate-session)
      (let ((result (funcall orig-fun body params)))
        (advice-remove 'org-babel-R-evaluate-session
                       'ob-comint-async-org-babel-R-evaluate-session)
        result))))

(advice-add 'org-babel-execute:R :around 'ob-comint-async-org-babel-execute:R)

(defconst ob-comint-async-R-indicator "'ob_comint_async_R_%s_%s'")

(defun ob-comint-async-org-babel-R-evaluate-session
    (session body result-type result-params column-names-p row-names-p)
  "Asynchronously evaluate BODY in SESSION.
Returns a placeholder string for insertion, to later be replaced
by `ob-comint-async-filter'."
  (ob-comint-async-register
   session (current-buffer)
   "^\\[1\\] \"ob_comint_async_R_\\(.+\\)_\\(.+\\)\"$"
   'ob-comint-async-R-output-callback
   'ob-comint-async-R-value-callback)
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
	  (mapconcat
           'org-babel-chomp
           (list (format org-babel-R-write-object-command
                         (if row-names-p "TRUE" "FALSE")
                         (if column-names-p
                             (if row-names-p "NA" "TRUE")
                           "FALSE")
                         ".Last.value"
                         (org-babel-process-file-name tmp-file 'noquote))
                 (format ob-comint-async-R-indicator
                         "file" tmp-file))
           "\n"))
	 (let ((ess-local-process-name
		(process-name (get-buffer-process session)))
	       (ess-eval-visibly-p nil))
	   (ess-eval-buffer nil)))
       tmp-file))
    (output
     (let ((uuid (md5 (number-to-string (random 100000000)))))
       (ob-comint-async-delete-dangling-and-eval
	   session
	 (insert (mapconcat 'org-babel-chomp
			    (list (format ob-comint-async-R-indicator
					  "start" uuid)
				  body
				  (format ob-comint-async-R-indicator
					  "end" uuid))
			    "\n"))
	 (inferior-ess-send-input))
       uuid))))

(defun ob-comint-async-R-output-callback (output)
  "Callback for async output results.
Assigned locally to `ob-comint-async-chunk-callback' in R
comint buffers used for asynchronous Babel evaluation."
  (mapconcat
   'org-babel-chomp
   (cdr
    (butlast
     (mapcar
      (lambda (line) (string-remove-prefix "\n" line))
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
              output))))))
   "\n"))

(defun ob-comint-async-R-value-callback (params tmp-file)
  "Callback for async value results.
Assigned locally to `ob-comint-async-file-callback' in R
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

(provide 'ob-comint-async-R)

;;; ob-comint-async-R.el ends here
