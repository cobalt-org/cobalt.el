;;; Test for `cobalt'

;;; Commentary:
;; These are the tests for `cobalt'

;;; Todo:
;; - Create a test for cobalt-serve without prefix argument

;;; Code:
(require 'f)

(ert-deftest t-cobalt-version-test ()
  (should (equal (shell-command-to-string "cobalt -V") "Cobalt 0.11.1\n")))

(ert-deftest t-cobalt-serve ()
  (within-sandbox
   (let* ((test-site-path (concat cobalt-sandbox-path "/test-site/"))
	  (cobalt-site-paths (list test-site-path))
	  (cobalt--current-site (car cobalt-site-paths))
	  (cobalt--serve-process nil))
     (unless (f-exists? test-site-path)
       (f-mkdir test-site-path))
     
     (should-not cobalt--serve-process)
     (cobalt-serve '(4))
     (should cobalt--serve-process)
     (cobalt-serve-kill)
     (should-not cobalt--serve-process))))

(ert-deftest t-cobalt-init ()
  (within-sandbox
   (let ((test-site-path (concat cobalt-sandbox-path "/test-site/")))
     (should-not (f-exists? (concat test-site-path "_cobalt.yml")))
     (cobalt--init test-site-path)
     (should (f-exists? (concat test-site-path "_cobalt.yml"))))))

(ert-deftest t-cobalt-new-post-and-publish ()
  (within-sandbox			;
   (let ((test-site-path (concat cobalt-sandbox-path "/test-site/")))
     (unless (f-exists? test-site-path)
       (f-mkdir test-site-path))
     (unless (f-exists? (concat test-site-path "posts/"))
       (f-mkdir (concat test-site-path "posts/") ))

     (let* ((cobalt-site-paths (list test-site-path))
	    (cobalt--current-site (car cobalt-site-paths))
	    (full-post-path (concat test-site-path "posts/this-is-a-test.md"))
	    (revert-without-query '(".*")))
       
       (cobalt--init test-site-path)
       
       (cobalt--new-post-with-title "This is a test" nil)
       (should (f-exists? full-post-path))

       (with-temp-buffer
	 (find-file full-post-path)
	 (goto-char (point-min))
	 (should (search-forward "is_draft: true" nil t))
	 (cobalt-publish))
       (with-temp-buffer
	 (find-file full-post-path)
	 (goto-char (point-min))
	 (should (search-forward "is_draft: false" nil t)))))))

(ert-deftest t-cobalt-utils ()
  (should (equal (cobalt--check-fix-site-path "test/blog/site") "test/blog/site/"))
  (should (equal (cobalt--check-fix-site-path "test/blog/site/") "test/blog/site/")))

(ert-deftest t-cobalt-convert-title-to-file-name ()
  (should (equal (cobalt--convert-title-to-file-name "This is a test") "this-is-a-test"))
  (should (equal (cobalt--convert-title-to-file-name "This is a test.") "this-is-a-test"))
  (should (equal (cobalt--convert-title-to-file-name "*)+=~{}This is-a  test.") "this-is-a-test")))
