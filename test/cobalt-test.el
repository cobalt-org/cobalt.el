;;; Test for `cobalt'

;;; Commentary:
;; These are the tests for `cobalt'

;;; Code:
(require 'f)

(ert-deftest t-cobalt-version-test ()
  (should (equal (shell-command-to-string "cobalt -V") "Cobalt 0.11.1\n") ))

(ert-deftest t-cobalt-serve ()
  (within-sandbox
   (let* ((test-blog-path (concat cobalt-sandbox-path "/test-blog/"))
	  (cobalt-site-paths (list test-blog-path))
	  (cobalt--current-site (car cobalt-site-paths))
	  (cobalt--serve-process nil))
     (unless (f-exists? test-blog-path)
       (f-mkdir test-blog-path))
     
     (should-not cobalt--serve-process)
     (cobalt-serve)
     (should cobalt--serve-process)
     (cobalt-serve-kill)
     (should-not cobalt--serve-process))))

(ert-deftest t-cobalt-new-post ()
  (within-sandbox
   (let ((test-blog-path (concat cobalt-sandbox-path "/test-blog/")))
     (unless (f-exists? test-blog-path)
       (f-mkdir test-blog-path))
     (unless (f-exists? (concat test-blog-path "posts/"))
       (f-mkdir (concat test-blog-path "posts/") ))

     (let* ((cobalt-site-paths (list test-blog-path))
	    (cobalt--current-site (car cobalt-site-paths)))
       (cobalt--new-post-with-title "This is a test" nil)
       (should (f-exists? (concat test-blog-path "posts/this-is-a-test.md"))))
     )
   ))

(ert-deftest t-cobalt-convert-title-to-file-name ()
  (should (equal (cobalt--convert-title-to-file-name "This is a test") "this-is-a-test"))
  (should (equal (cobalt--convert-title-to-file-name "This is a test.") "this-is-a-test"))
  (should (equal (cobalt--convert-title-to-file-name "*)+=~{}This is-a  test.") "this-is-a-test")))
