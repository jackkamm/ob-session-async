;;; ob-session-async-sh.el --- Async Babel Interaction with sh sessions -*- lexical-binding: t -*-

;; Copyright (C) 2019

;; Author:  <myrl.0xf@gmail.com>

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

;; Support for evaluating sh code asynchronously in org-babel sessions.

;;; Code:

(require 'ob-session-async)
(require 'subr-x)

(defun ob-session-async-org-babel-execute:sh (orig-fun body params)
  "Advice around `org-babel-execute:sh' to enable asynchronous evaluation."
  (let ((async (assq :async params))
        (session (assq :session params)))
    (if (or (not async)
            (equal (cdr async) "no")
            (equal (cdr session) "none"))
        (funcall orig-fun body params)
      (progn
        (advice-add 'org-babel-sh-evaluate
                    :override 'ob-session-async-org-babel-sh-evaluate-session)
        (let ((result (funcall orig-fun body params)))
          (advice-remove 'org-babel-sh-evaluate
                         'ob-session-async-org-babel-sh-evaluate-session)
          result)))))

(advice-add 'org-babel-execute:sh :around 'ob-session-async-org-babel-execute:sh)

(defconst ob-session-async-sh-indicator "printf 'ob_comint_async_sh_%s_%s'")

(defun ob-session-async-org-babel-sh-evaluate-session
    (session body &optional params stdin cmdline)
  "Asynchronously evaluate BODY in SESSION.
Returns a placeholder string for insertion, to later be replaced
by `ob-session-async-filter'."
  (ob-session-async-register
   session (current-buffer)
   "^ob_comint_async_sh_\\(.+\\)_\\(.+\\)$"
   'org-babel-chomp
   'ob-session-async-sh-value-callback)
  (let* ((shebang (cdr (assq :shebang params)))
         (uuid (md5 (number-to-string (random 100000000))))
         (prompt (format "%s>" uuid)))
    (with-current-buffer (generate-new-buffer "*sh-temp*")
      (let ((temp-buffer (current-buffer)))
        (insert (concat "PS2=$'\\n" prompt "'\n"))
        (insert (format ob-session-async-sh-indicator
                        "start" uuid))
        (insert "\n")
        (insert body)
        (insert "\n")
        (insert (format ob-session-async-sh-indicator
                        "end" uuid))
        (insert "\n")

        (cl-labels ((send-line (prompt?)
                               (when (equal `(,prompt) (last (split-string prompt? "\n")))
                                 (with-current-buffer temp-buffer
                                   (beginning-of-buffer)
                                   (condition-case nil
                                       (progn
                                         (setq last-command 'nil)
                                         (kill-whole-line)
                                         (org-babel-comint-in-buffer session
                                                                     (goto-char (process-mark (get-buffer-process (current-buffer))))
                                                                     (yank)
                                                                     (delete-backward-char 1)
                                                                     (comint-send-input)))
                                     ((end-of-buffer)
                                      (kill-buffer temp-buffer)
                                      (remove-hook 'comint-output-filter-functions #'send-line)))))
                               ""))
          (org-babel-comint-in-buffer session
                                      (add-hook 'comint-output-filter-functions #'send-line)
                                      (goto-char (process-mark (get-buffer-process (current-buffer))))
                                      (insert (concat "PS1=$'\\n" prompt "'"))
                                      (comint-send-input)))))
    uuid))

(defun ob-session-async-sh-value-callback (params tmp-file)
  "Callback for async value results.
Assigned locally to `ob-session-async-file-callback' in sh
comint buffers used for asynchronous Babel evaluation."
  (let* ((graphics-file (and (member "graphics" (assq :result-params params))
                             (org-babel-graphical-output-file params)))
         (colnames-p (unless graphics-file (cdr (assq :colnames params)))))
    (org-babel-sh-process-value-result
     (org-babel-result-cond (assq :result-params params)
                            (with-temp-buffer
                              (insert-file-contents tmp-file)
                              (org-babel-chomp (buffer-string) "\n"))
                            (org-babel-import-elisp-from-file tmp-file '(16)))
     (or (equal "yes" colnames-p)
         (org-babel-pick-name
          (cdr (assq :colname-names params)) colnames-p)))))

(provide 'ob-session-async-sh)

;;; ob-session-async-R.el ends here
