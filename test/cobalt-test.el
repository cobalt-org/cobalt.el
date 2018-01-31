;;; Test for `cobalt'

;;; Commentary:
;; These are the tests for `cobalt'

;;; Code:
(require 'f)

(ert-deftest t-cobalt-version-test ()
  (should (equal (shell-command-to-string "cobalt -V") "Cobalt 0.11.1\n") ))

(ert-deftest t-cobalt-main ()
  (within-sandbox
   (setq-local test-blog-path (concat cobalt-sandbox-path "/test-blog/"))
   (unless (f-exists? test-blog-path)
     (f-mkdir test-blog-path))
   
   (setq-local cobalt-site-paths (list test-blog-path))
   (setq-local cobalt--current-site (car cobalt-site-paths))
   
   (setq-local cobalt--serve-process nil)
   (should-not cobalt--serve-process)
   (cobalt-serve)
   (should cobalt--serve-process)
   (cobalt-serve-kill)
   (should-not cobalt--serve-process)))

(ert-deftest t-convert-title-to-file-name ()
  (should (equal (cobalt--convert-title-to-file-name "This is a test") "this-is-a-test"))
  (should (equal (cobalt--convert-title-to-file-name "This is a test.") "this-is-a-test"))
  (should (equal (cobalt--convert-title-to-file-name "*)+=~{}This is-a  test.") "this-is-a-test")))
