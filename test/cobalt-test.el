;;; Test for `cobalt'

;;; Commentary:
;; These are the tests for `cobalt'

;;; Code:

(ert-deftest t-convert-title-to-file-name ()
  (should (equal (cobalt--convert-title-to-file-name "This is a test") "this-is-a-test"))
  (should (equal (cobalt--convert-title-to-file-name "This is a test.") "this-is-a-test"))
  (should (equal (cobalt--convert-title-to-file-name "*)+=~{}This is-a  test.") "this-is-a-test")))
