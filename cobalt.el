;;; cobalt.el --- Summary
;;; Commentary:
;;; Code:

(defvar cobalt-blog-paths '("~/blogs/accidentalrebel.github.com/" "~/blogs/karlolicudine-blog/")
  "List of blogs of the user.")

(defvar cobalt-log-buffer-name "*cobalt*"
  "Name of the log buffer for cobalt process output.")

(defvar cobalt--serve-process nil
  "Use to save cobalt serve process is so it can be killed in the future.")

(defun cobalt-version ()
  "Initialize cobalt."
  (interactive)
  (let ((default-directory (car cobalt-blog-paths)))
    (message "%s" (shell-command-to-string (concat (executable-find "cobalt") " -V")))))

(defun cobalt-serve ()
  "Build, serve, and watch the project at the source dir."
  (interactive)
  (let* ((default-directory (car cobalt-blog-paths)))
    (setq cobalt--serve-process (start-process "cobalt-serve" cobalt-log-buffer-name (executable-find "cobalt") "serve"))
    (when (not cobalt--serve-process)
      (message "Error in running: cobalt serve"))))

(defun cobalt-serve-kill ()
  "Kill the cobalt serve process, if existing."
  (interactive)
  (when cobalt--serve-process
    (kill-process cobalt--serve-process)))

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
