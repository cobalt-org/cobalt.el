;;; Test for `cobalt'

;;; Commentary:
;; These are the tests for `cobalt'

;;; Code:

(ert-deftest t-cobalt-version-test ()
  (should (equal (shell-command-to-string "cobalt -V") "Cobalt 0.11.1\n") ))

(ert-deftest t-convert-title-to-file-name ()
  (should (equal (cobalt--convert-title-to-file-name "This is a test") "this-is-a-test"))
  (should (equal (cobalt--convert-title-to-file-name "This is a test.") "this-is-a-test"))
  (should (equal (cobalt--convert-title-to-file-name "*)+=~{}This is-a  test.") "this-is-a-test")))
