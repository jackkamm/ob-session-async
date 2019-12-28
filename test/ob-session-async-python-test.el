;;; ob-session-async-python-test.el --- Tests for ob-session-async

(require 'org-test)
(require 'python)
(require 'ob-session-async-python)

(ert-deftest ob-session-async-python-simple-session-async-output ()
  (let ((org-babel-temporary-directory "/tmp")
        (org-confirm-babel-evaluate nil))
    (org-test-with-temp-text
     "#+begin_src python :session python :async yes :results output\nimport time\ntime.sleep(.1)\nprint('Yep!')\n#+end_src\n"
     (should (let ((expected "Yep!"))
	       (and (not (string= expected (org-babel-execute-src-block)))
		    (string= expected
			     (progn
			       (sleep-for 0 200)
			       (goto-char (org-babel-where-is-src-block-result))
			       (org-babel-read-result)))))))))

(ert-deftest ob-session-async-python-named-output ()
  (let (org-confirm-babel-evaluate
        (org-babel-temporary-directory "/tmp")
        (src-block "#+begin_src python :async :session R :results output\n  print(\"Yep!\")\n#+end_src")
        (results-before "\n\n#+NAME: foobar\n#+RESULTS:\n: Nope!")
        (results-after "\n\n#+NAME: foobar\n#+RESULTS:\n: Yep!\n"))
    (org-test-with-temp-text
     (concat src-block results-before)
     (should (progn (org-babel-execute-src-block)
                    (sleep-for 0 200)
                    (string= (concat src-block results-after)
                             (buffer-string)))))))

(ert-deftest ob-session-async-python-output-drawer ()
  (let (org-confirm-babel-evaluate
        (org-babel-temporary-directory "/tmp")
        (src-block "#+begin_src python :async :session python :results output drawer\n  print(list(range(3)))\n#+end_src")
        (result "\n\n#+RESULTS:\n:results:\n[0, 1, 2]\n:end:\n"))
    (org-test-with-temp-text
     src-block
     (should (progn (org-babel-execute-src-block)
                    (sleep-for 0 200)
                    (string= (concat src-block result)
                             (buffer-string)))))))

;;; ob-session-async-python-test.el ends here
