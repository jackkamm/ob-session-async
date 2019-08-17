;;; ob-session-async.el --- Asynchronous Babel Interaction with Comint Buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <jackkamm@gmail.com>
;; Keywords: outlines, languages
;; Version: 0.1.0

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

(with-eval-after-load 'ob-async
  (defun ob-session-async-org-babel-execute-src-block
      (orig-advice orig-fun &optional arg info params)
    "Advice to wrap around `ob-async-org-babel-execute-src-block'.
That function is itself an advice around `org-babel-execute-src-block'."
    (let ((block-info (nth 2 (or info (org-babel-get-src-block-info)))))
      ;; if no :session, use the original ob-async advice
      (if (equal (cdr (assoc :session block-info)) "none")
          (funcall orig-advice orig-fun arg info params)
        ;; else, skip it
        (funcall orig-fun arg info params))))

  (advice-add 'ob-async-org-babel-execute-src-block
              :around 'ob-session-async-org-babel-execute-src-block))

(defvar-local ob-session-async-indicator nil
  "Regular expression that `ob-session-async-filter' scans for.
It should have 2 parenthesized expressions,
e.g. \"org_babel_async_\\(start\\|end\\|file\\)_\\(.*\\)\". The
first parenthesized expression determines whether the token is
delimiting a result block, or whether the result is in a file. If
delimiting a block, the second expression gives a UUID for the
location to insert the result. Otherwise, the result is in a tmp
file, and the second expression gives the file name.")

(defvar-local ob-session-async-buffers nil
  "List of org-mode buffers to check for Babel async output results.")

(defvar-local ob-session-async-file-callback nil
  "Callback to clean and insert Babel async results from a temp file.
The callback function takes two arguments: the alist of params of the Babel
source block, and the name of the temp file.")

(defvar-local ob-session-async-chunk-callback nil
  "Callback to clean Babel async output results before insertion.
The input is assumed to be split by `comint-prompt-regexp', as in
 `org-babel-comint-with-output'. The output should be a string.")

(defvar-local ob-session-async-dangling nil
  "Dangling piece of the last process output, in case
`ob-session-async-indicator' is spread across multiple
comint outputs due to buffering.")

(defun ob-session-async-filter (string)
  "Captures Babel async output from comint buffer back to org-mode buffers.
This function is added as a hook to `comint-output-filter-functions'.
STRING contains the output originally inserted into the comint buffer."
  ;; Remove outdated org-mode buffers
  (setq ob-session-async-buffers
	(cl-loop for buf in ob-session-async-buffers
	      if (buffer-live-p buf)
	      collect buf))
  (let* ((indicator ob-session-async-indicator)
	 (org-buffers ob-session-async-buffers)
	 (file-callback ob-session-async-file-callback)
	 (combined-string (concat ob-session-async-dangling string))
	 (new-dangling combined-string)
	 ;; list of UUID's matched by `ob-session-async-indicator'
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
                             (let* ((info (org-babel-get-src-block-info))
                                    (params (nth 2 info))
                                    (result-params
                                     (cdr (assq :result-params params))))
                               (org-babel-insert-result
                                 (funcall file-callback
                                          (nth
                                           2 (org-babel-get-src-block-info))
                                          tmp-file)
                                result-params info))
			     t))))))))
      ;; Truncate dangling to only the most recent output
      (when (> (length new-dangling) (length string))
	(setq new-dangling string)))
    (setq-local ob-session-async-dangling new-dangling)
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
		     ;; move point to beginning of indicator
                     (- (match-beginning 0) 1)
		     ;; find the matching start indicator
		     (cl-loop for pos = (re-search-backward indicator)
			   until (and (equal (match-string 1) "start")
				      (equal (match-string 2) uuid))
			   finally return (+ 1 (match-end 0)))))
		   ;; Apply callback to clean up the result
		   (res-str (funcall ob-session-async-chunk-callback
                                     res-str-raw)))
	      ;; Search for uuid in associated org-buffers to insert results
	      (cl-loop for buf in org-buffers
		    until (with-current-buffer buf
			    (save-excursion
			      (goto-char (point-min))
			      (when (search-forward uuid nil t)
				(org-babel-previous-src-block)
                                (let* ((info (org-babel-get-src-block-info))
                                       (params (nth 2 info))
                                       (result-params
                                        (cdr (assq :result-params params))))
				  (org-babel-insert-result
                                   res-str result-params info))
				t))))
	      ;; Remove uuid from the list to search for
	      (setq uuid-list (delete uuid uuid-list)))))))))

(defun ob-session-async-register
    (session-buffer org-buffer indicator-regexp
		    chunk-callback file-callback)
  "Sets local ob-session-async variables in SESSION-BUFFER.
ORG-BUFFER is added to `ob-session-async-buffers' if not
present.  `ob-session-async-indicator',
`ob-session-async-chunk-callback', and
`ob-session-async-file-callback' are set to
INDICATOR-REGEXP, CHUNK-CALLBACK, and FILE-CALLBACK
respectively."
  (org-babel-comint-in-buffer session-buffer
    (setq ob-session-async-indicator indicator-regexp
	  ob-session-async-chunk-callback chunk-callback
	  ob-session-async-file-callback file-callback)
    (unless (memq org-buffer ob-session-async-buffers)
      (setq ob-session-async-buffers
	    (cons org-buffer ob-session-async-buffers)))
    (add-hook 'comint-output-filter-functions
	      'ob-session-async-filter nil t)))

(defmacro ob-session-async-delete-dangling-and-eval
    (session-buffer &rest body)
  "Remove dangling text in SESSION-BUFFER and evaluate BODY.
This is analogous to `org-babel-comint-with-output', but meant
for asynchronous output, and much shorter because inserting the
result is delegated to `ob-session-async-filter'."
  (declare (indent 1))
  `(org-babel-comint-in-buffer ,session-buffer
     (goto-char (process-mark (get-buffer-process (current-buffer))))
     (delete-region (point) (point-max))
     ,@body))
(def-edebug-spec ob-session-async-with-output (sexp body))

(provide 'ob-session-async)
;;; ob-session-async.el ends here
