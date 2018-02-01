;;; cobalt.el --- Summary
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Todo:
;; - Add a way to specify if draft or not for cobalt-serve
;; - Create a cobalt-command function

(defvar cobalt-site-paths '("~/blogs/accidentalrebel.github.com/" "~/blogs/testblog/")
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
      (setq cobalt--serve-process (start-process "cobalt-serve" cobalt-log-buffer-name (executable-find "cobalt") "serve" "--drafts"))
      (if (not cobalt--serve-process)
	  (message "Error in running: cobalt serve")
	(message "Serve process is now running.")))))

(defun cobalt-serve-kill ()
  "Kill the cobalt serve process, if existing."
  (interactive)
  (when cobalt--serve-process
    (kill-process cobalt--serve-process))
  (setq cobalt--serve-process nil))

(defun cobalt-new-post (post-title)
  "Ask for POST-TITLE and create a new post."
  (interactive "sWhat is the title of the post? ")
  (cobalt--new-post-with-title post-title t))

(defun cobalt-preview-current-post ()
  "Opens the current buffer."
  (interactive)
  (if (not cobalt--serve-process)
      (message "No serve process is currently running! Call cobalt-serve first!")
    (let* ((post-path (concat (car (butlast (split-string (buffer-name) "\\."))) ".html"))
	   (full-url (concat "http://127.0.0.1:3000/posts/" post-path)))
      (message "Previewing post: %s" full-url)
      (browse-url full-url))))

(defun cobalt-preview-site ()
  "Preview the site."
  (interactive)
  (if (not cobalt--serve-process)
      (message "No serve process is currently running! Call cobalt-serve first!")
    (browse-url "http://127.0.0.1:3000")))

(defun cobalt--new-post-with-title (post-title open-file-on-success)
  "Create a new post with POST-TITLE."
  (when (not cobalt--current-site)
    (cobalt-change-current-site))
  (let ((default-directory cobalt--current-site)
	(posts-directory "posts/")
	(post-file-name (cobalt--convert-title-to-file-name post-title)))
    (apply 'call-process (executable-find "cobalt") nil cobalt-log-buffer-name nil (list "new" "-f" posts-directory post-title))
    (when open-file-on-success
      (if (not (file-exists-p (concat default-directory posts-directory post-file-name ".md")))
	  (message "Could not find file: %s." (concat default-directory posts-directory post-file-name ".md"))
	(find-file (concat default-directory posts-directory post-file-name ".md"))))))

(defun cobalt--convert-title-to-file-name (post-title)
  "Convert the given POST-TITLE to a file name."
  (downcase (replace-regexp-in-string "^-\\|-$"
				      ""
				      (replace-regexp-in-string "--+"
							       "-"
							       (replace-regexp-in-string "[^A-Za-z0-9]"
											 "-"
											 post-title)))))

(provide 'cobalt)
;;; cobalt.el ends here
