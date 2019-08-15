;;; ob-session-async-test.el --- Tests for ob-session-async

(require 'org-test)
(require 'ess-r-mode)
(require 'ob-session-async-R)

(ert-deftest ob-session-async-R-simple-session-async-value ()
  (let (ess-ask-for-ess-directory
        ess-history-file
        (org-confirm-babel-evaluate nil))
    (org-test-with-temp-text
     "#+begin_src R :session R :async yes\n  Sys.sleep(.1)\n  paste(\"Yep!\")\n#+end_src\n"
     (should (let ((expected "Yep!"))
	       (and (not (string= expected (org-babel-execute-src-block)))
		    (string= expected
			     (progn
			       (sleep-for 0 200)
			       (goto-char (org-babel-where-is-src-block-result))
			       (org-babel-read-result)))))))))

(ert-deftest ob-session-async-R-simple-session-async-output ()
  (let (ess-ask-for-ess-directory
        ess-history-file
        (org-confirm-babel-evaluate nil))
    (org-test-with-temp-text
     "#+begin_src R :session R :results output :async yes\n  Sys.sleep(.1)\n  1:5\n#+end_src\n"
     (should (let ((expected "[1] 1 2 3 4 5"))
	       (and (not (string= expected (org-babel-execute-src-block)))
		    (string= expected
			     (progn
			       (sleep-for 0 200)
			       (goto-char (org-babel-where-is-src-block-result))
			       (org-babel-read-result)))))))))

(ert-deftest ob-session-async-R-named-output ()
  (let (ess-ask-for-ess-directory
        ess-history-file
        org-confirm-babel-evaluate
        (src-block "#+begin_src R :async :session R :results output\n  1:5\n#+end_src")
        (results-before "\n\n#+NAME: foobar\n#+RESULTS:\n: [1] 1")
        (results-after "\n\n#+NAME: foobar\n#+RESULTS:\n: [1] 1 2 3 4 5\n"))
    (org-test-with-temp-text
     (concat src-block results-before)
     (should (progn (org-babel-execute-src-block)
                    (sleep-for 0 200)
                    (string= (concat src-block results-after)
                             (buffer-string)))))))

;;; ob-session-async-test.el ends here
