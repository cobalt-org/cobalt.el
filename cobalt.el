;;; cobalt.el --- Summary
;;; Commentary:
;;; Code:

(defvar cobalt-site-paths '("~/blogs/accidentalrebel.github.com/" "~/blogs/karlolicudine-blog/")
  "List of site of the user.")

(defvar cobalt-log-buffer-name "*cobalt*"
  "Name of the log buffer for cobalt process output.")

(defvar cobalt--serve-process nil
  "Use to save cobalt serve process is so it can be killed in the future.")

(defvar cobalt--current-site nil
  "The current site.")

(defun cobalt-change-current-site ()
  "Show a selection to switch current site.

Kills an exiting server process.  User should run cobalt-serve again for the newly switch site."
  (interactive)
  (when cobalt--serve-process
    (cobalt-serve-kill)
    (message "Server killed for %s" cobalt--current-site))
  (when (and cobalt-site-paths (> (length cobalt-site-paths) 0) )
    (setq cobalt--current-site (completing-read "Select site to use as current: " cobalt-site-paths nil t))))

(defun cobalt-serve ()
  "Build, serve, and watch the project at the source dir."
  (interactive)
  (if cobalt--serve-process
      (message "Serve process already running!")
    (when (not cobalt--current-site)
      (cobalt-change-current-site))
    (let* ((default-directory cobalt--current-site))
      (setq cobalt--serve-process (start-process "cobalt-serve" cobalt-log-buffer-name (executable-find "cobalt") "serve"))
      (when (not cobalt--serve-process)
	(message "Error in running: cobalt serve")))))

(defun cobalt-serve-kill ()
  "Kill the cobalt serve process, if existing."
  (interactive)
  (when cobalt--serve-process
    (kill-process cobalt--serve-process)
    (setq cobalt--serve-process nil)))

(defun cobalt-new-post (post-title)
  "Initialize cobalt with POST-TITLE."
  (interactive "sWhat is the title of the post? ")
  (when (not cobalt--current-site)
    (cobalt-change-current-site))
  (let* ((default-directory cobalt--current-site)
	 (posts-directory "posts/")
	 (new-command-string (concat (executable-find "cobalt") " new -f " posts-directory " " post-title)))
    (message "the command is %s" new-command-string)
    ;;(message "Returned: %s" (shell-command-to-string new-command-string))
    (shell-command new-command-string cobalt-log-buffer-name cobalt-log-buffer-name)
    (when (file-exists-p (concat posts-directory post-title ".md"))
      (find-file (concat posts-directory post-title ".md")))))

(provide 'cobalt)
;;; cobalt.el ends here
