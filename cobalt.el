;;; cobalt.el --- Summary
;;; Commentary:
;;; Code:

(defvar cobalt-blog-paths '("~/blogs/accidentalrebel.github.com/" "~/blogs/karlolicudine-blog/"))
(defvar cobalt-log-buffer-name "*cobalt*")

(defun cobalt-version ()
  "Initialize cobalt."
  (interactive)
  (let ((default-directory (car cobalt-blog-paths)))
    (message "%s" (shell-command-to-string (concat (executable-find "cobalt") " -V")))))

(defun cobalt-new-post (post-title)
  "Initialize cobalt with POST-TITLE."
  (interactive "sWhat is the title of the post? ")
  (let* ((default-directory (car cobalt-blog-paths))
	 (posts-directory "posts/")
	 (new-command-string (concat (executable-find "cobalt") " new -f " posts-directory " " post-title)))
    (message "the command is %s" new-command-string)
    ;;(message "Returned: %s" (shell-command-to-string new-command-string))
    (shell-command new-command-string cobalt-log-buffer-name cobalt-log-buffer-name)
    (when (file-exists-p (concat posts-directory post-title ".md"))
      (find-file (concat posts-directory post-title ".md")))))

(provide 'cobalt)
;;; cobalt.el ends here
