;;; test-helper.el --- Helpers for ob-session-async-test.el

(when (> emacs-major-version 26)
  (defalias 'ert--print-backtrace 'backtrace-to-string))

;;; test-helper.el ends here
