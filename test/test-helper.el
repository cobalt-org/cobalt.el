;;; test-helper --- Test helper for cobalt

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar cobalt-test-path
  (f-dirname (f-this-file)))

(defvar cobalt-root-path
  (f-parent cobalt-test-path))

(defvar cobalt-sandbox-path
  (f-expand "sandbox" cobalt-test-path))

(when (f-exists? cobalt-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" cobalt-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory cobalt-sandbox-path))
     (when (f-exists? cobalt-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir cobalt-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'el-mock)
(eval-when-compile
  (require 'cl))
(require 'cobalt)

;;; test-helper.el ends here
