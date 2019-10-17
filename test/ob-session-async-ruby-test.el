;;; ob-session-async-ruby-test.el --- Tests for ob-session-async

(require 'org-test)
(require 'ob-session-async-ruby)

(ert-deftest ob-session-async-ruby-simple-session-async-value ()
  (let ((org-babel-temporary-directory "/tmp")
         (org-confirm-babel-evaluate nil))
    (org-test-with-temp-text
      "#+begin_src ruby :async :session ruby :results value\n\"Yep!\"\n#+end_src\n"
      (should (let ((expected "Yep!"))
                (string= expected
                  (progn
                    (org-babel-execute-src-block)
                    (sleep-for 0 300)
                    (goto-char (org-babel-where-is-src-block-result))
                    (org-babel-read-result))))))))

(ert-deftest ob-session-async-ruby-simple-session-async-output ()
  (let ((org-babel-temporary-directory "/tmp")
         (org-confirm-babel-evaluate nil))
    (org-test-with-temp-text
      "#+begin_src ruby :async :session ruby :results output\np [1, 2, 3, 4]\n#+end_src\n"
      (should (let ((expected "[1, 2, 3, 4]"))
                (and (not (string= expected (org-babel-execute-src-block)))
                  (string-match-p expected
                    (progn
                      (sleep-for 0 400)
                      (goto-char (org-babel-where-is-src-block-result))
                      (org-babel-read-result)))))))))

(ert-deftest ob-session-async-ruby-named-value ()
  (let ((org-babel-temporary-directory "/tmp")
         (org-confirm-babel-evaluate nil)
         (src-block "#+begin_src ruby :async :session ruby :results value\n\"Yep!\"\n#+end_src")
         (results-before "\n\n#+NAME: foobar\n#+RESULTS:\n: [1] 1")
         (results-after "\n\n#+NAME: foobar\n#+RESULTS:\n: Yep!\n"))
    (org-test-with-temp-text
      (concat src-block results-before)
      (should (progn (org-babel-execute-src-block)
                (sleep-for 0 400)
                (string= (concat src-block results-after)
                  (buffer-string)))))))

(ert-deftest ob-session-async-ruby-output-drawer ()
  (let ((org-babel-temporary-directory "/tmp")
         (org-confirm-babel-evaluate nil)
         (src-block "#+begin_src ruby :async :session ruby :results output drawer\np [1, 2, 3, 4]\n#+end_src")
         (result "\n\n#+RESULTS:\n:results:\n[1, 2, 3, 4]\n:end:\n"))
    (org-test-with-temp-text
      src-block
      (should (progn (org-babel-execute-src-block)
                (sleep-for 0 400)
                (string= (concat src-block result)
                  (buffer-string)))))))

(ert-deftest ob-session-async-ruby-value-drawer ()
  (let ((org-babel-temporary-directory "/tmp")
         (org-confirm-babel-evaluate nil)
         (src-block "#+begin_src ruby :async :session ruby :results value drawer\n  [1, 2, 3, 4]\n#+end_src")
         (result "\n\n#+RESULTS:\n:results:\n[1, 2, 3, 4]\n:end:\n"))
    (org-test-with-temp-text
      src-block
      (should (progn (org-babel-execute-src-block)
                (sleep-for 0 400)
                (string= (concat src-block result)
                  (buffer-string)))))))

;;; ob-session-async-ruby-test.el ends here
