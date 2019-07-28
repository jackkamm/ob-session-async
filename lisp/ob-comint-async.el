;;; ob-comint-async.el --- Asynchronous Babel Interaction with Comint Buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <jackkamm@gmail.com>
;; Keywords: outlines, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(require 'ob-comint)
(require 'ob-async)

(defun ob-comint-async-org-babel-execute-src-block
    (orig-advice orig-fun &optional arg info params)
  (let ((block-info (nth 2 (or info (org-babel-get-src-block-info)))))
    ;; if no :session, use the original ob-async advice
    (if (equal (cdr (assoc :session block-info)) "none")
        (funcall orig-advice orig-fun arg info params)
      ;; else, skip it
      (funcall orig-fun arg info params))))

(advice-add 'ob-async-org-babel-execute-src-block
            :around 'ob-comint-async-org-babel-execute-src-block)

(defvar-local ob-comint-async-indicator nil
  "Regular expression that `ob-comint-async-filter' scans for.
It should have 2 parenthesized expressions,
e.g. \"org_babel_async_\\(start\\|end\\|file\\)_\\(.*\\)\". The
first parenthesized expression determines whether the token is
delimiting a result block, or whether the result is in a file. If
delimiting a block, the second expression gives a UUID for the
location to insert the result. Otherwise, the result is in a tmp
file, and the second expression gives the file name.")

(defvar-local ob-comint-async-buffers nil
  "List of org-mode buffers to check for Babel async output results.")

(defvar-local ob-comint-async-file-callback nil
  "Callback to clean and insert Babel async results from a temp file.
The callback function takes two arguments: the alist of params of the Babel
source block, and the name of the temp file.")

(defvar-local ob-comint-async-chunk-callback nil
  "Callback to clean Babel async output results before insertion.
The input is assumed to be split by `comint-prompt-regexp', as in
 `org-babel-comint-with-output'. The output should be a string.")

(defvar-local ob-comint-async-dangling nil
  "Dangling piece of the last process output, in case
`ob-comint-async-indicator' is spread across multiple
comint outputs due to buffering.")

(defun ob-comint-async-filter (string)
  "Captures Babel async output from comint buffer back to org-mode buffers.
This function is added as a hook to `comint-output-filter-functions'.
STRING contains the output originally inserted into the comint buffer."
  ;; Remove outdated org-mode buffers
  (setq ob-comint-async-buffers
	(cl-loop for buf in ob-comint-async-buffers
	      if (buffer-live-p buf)
	      collect buf))
  (let* ((indicator ob-comint-async-indicator)
	 (org-buffers ob-comint-async-buffers)
	 (file-callback ob-comint-async-file-callback)
	 (combined-string (concat ob-comint-async-dangling string))
	 (new-dangling combined-string)
	 ;; list of UUID's matched by `ob-comint-async-indicator'
	 uuid-list)
    (with-temp-buffer
      (insert combined-string)
      (goto-char (point-min))
      (while (re-search-forward indicator nil t)
	;; update dangling
	(setq new-dangling (buffer-substring (point) (point-max)))
	(cond ((equal (match-string 1) "end")
	       ;; save UUID for insertion later
	       (push (match-string 2) uuid-list))
	      ((equal (match-string 1) "file")
	       ;; insert results from tmp-file
	       (let ((tmp-file (match-string 2)))
		 (cl-loop for buf in org-buffers
		       until
		       (with-current-buffer buf
			 (save-excursion
			   (goto-char (point-min))
			   (when (search-forward tmp-file nil t)
			     (org-babel-previous-src-block)
			     (org-babel-remove-result)
			     (org-babel-insert-result
			      (funcall file-callback
				       (nth
					2 (org-babel-get-src-block-info))
				       tmp-file))
			     t))))))))
      ;; Truncate dangling to only the most recent output
      (when (> (length new-dangling) (length string))
	(setq new-dangling string)))
    (setq-local ob-comint-async-dangling new-dangling)
    (when uuid-list
      ;; Search for results in the comint buffer
      (save-excursion
	(goto-char (point-max))
	(while uuid-list
	  (re-search-backward indicator)
	  (when (equal (match-string 1) "end")
	    (let* ((uuid (match-string-no-properties 2))
		   (res-str-raw
		    (buffer-substring
		     ;; move point to end of indicator
		     (re-search-forward indicator)
		     ;; find the matching start indicator
		     (cl-loop for pos = (re-search-backward indicator)
			   until (and (equal (match-string 1) "start")
				      (equal (match-string 2) uuid))
			   finally return pos)))
		   ;; Apply callback to clean up the result
		   (res-str (funcall ob-comint-async-chunk-callback
				     (split-string
				      res-str-raw
				      comint-prompt-regexp))))
	      ;; Search for uuid in associated org-buffers to insert results
	      (cl-loop for buf in org-buffers
		    until (with-current-buffer buf
			    (save-excursion
			      (goto-char (point-min))
			      (when (search-forward uuid nil t)
				(org-babel-previous-src-block)
				(org-babel-remove-result)
				(org-babel-insert-result res-str)
				t))))
	      ;; Remove uuid from the list to search for
	      (setq uuid-list (delete uuid uuid-list)))))))))

(defun ob-comint-async-register
    (session-buffer org-buffer indicator-regexp
		    chunk-callback file-callback)
  "Sets local ob-comint-async variables in SESSION-BUFFER.
ORG-BUFFER is added to `ob-comint-async-buffers' if not
present.  `ob-comint-async-indicator',
`ob-comint-async-chunk-callback', and
`ob-comint-async-file-callback' are set to
INDICATOR-REGEXP, CHUNK-CALLBACK, and FILE-CALLBACK
respectively."
  (org-babel-comint-in-buffer session-buffer
    (setq ob-comint-async-indicator indicator-regexp
	  ob-comint-async-chunk-callback chunk-callback
	  ob-comint-async-file-callback file-callback)
    (unless (memq org-buffer ob-comint-async-buffers)
      (setq ob-comint-async-buffers
	    (cons org-buffer ob-comint-async-buffers)))
    (add-hook 'comint-output-filter-functions
	      'ob-comint-async-filter nil t)))

(defmacro ob-comint-async-delete-dangling-and-eval
    (session-buffer &rest body)
  "Remove dangling text in SESSION-BUFFER and evaluate BODY.
This is analogous to `org-babel-comint-with-output', but meant
for asynchronous output, and much shorter because inserting the
result is delegated to `ob-comint-async-filter'."
  (declare (indent 1))
  `(org-babel-comint-in-buffer ,session-buffer
     (goto-char (process-mark (get-buffer-process (current-buffer))))
     (delete-region (point) (point-max))
     ,@body))
(def-edebug-spec ob-comint-async-with-output (sexp body))

(provide 'ob-comint-async)
;;; ob-comint-async.el ends here
