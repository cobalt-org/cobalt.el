;;; cobalt.el --- Summary   -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;;; Todo:
;; - Create a function to log package messages to cobalt-log-buffer-name.
;; - Only preview a buffer if it is a valid post.
;; - If post is a draft, and cobalt-serve was not run with "--drafts", then don't allow previewing.
;; - If start-process returns an error don't let it set cobalt--serve-process
;; - Should be able to set which port to serve from.

(defcustom cobalt-site-paths nil
  "List of sites."
  :group 'cobalt
  :type 'sexp)

(defcustom cobalt-log-buffer-name "*cobalt*"
  "Name of the log buffer for cobalt process output."
  :group 'cobalt
  :type 'string)

(defvar cobalt--serve-process nil
  "Use to save cobalt serve process is so it can be killed in the future.")

(defvar cobalt--current-site nil
  "The current site to run cobalt commands on.")

(defun cobalt-comand (args)
  "Run specified cobalt command with ARGS at the current folder of the specified site."
  (interactive "scobalt ")
  (when (cobalt--executable-exists-p)
    (when (not cobalt--current-site)
      (cobalt-change-current-site))
    (let ((command-args (split-string args " ")))
      (apply 'call-process (executable-find "cobalt")
			   nil
			   cobalt-log-buffer-name
			   nil
			   command-args))
    (pop-to-buffer cobalt-log-buffer-name)))

(defun cobalt-change-current-site ()
  "Show a selection to switch current site.
Kills an exiting server process.  User should run cobalt-serve again for the newly switch site."
  (interactive)
  (when (cobalt--executable-exists-p)
    (when cobalt--serve-process
      (cobalt-serve-kill)
      (message "Server killed for %s" cobalt--current-site))
    (if (and cobalt-site-paths (> (length cobalt-site-paths) 0) )
	(setq cobalt--current-site (completing-read "Select site to use as current: " cobalt-site-paths nil t))
      (message "cobalt-site-paths is empty! Set it first."))))

(defun cobalt-serve (arg)
  "Build, serve, and watch the project at the source dir.
Specify a prefix argument (c-u) as ARG to include drafts."
  (interactive "P")
  (when (cobalt--executable-exists-p)
    (if cobalt--serve-process
	(message "Serve process already running!")
      (when (not cobalt--current-site)
	(cobalt-change-current-site))
      (let* ((default-directory cobalt--current-site))
	(setq cobalt--serve-process (start-process "cobalt-serve"
						   cobalt-log-buffer-name
						   (executable-find "cobalt")
						   "serve"
						   (if (equal arg '(4))
						       "--drafts"
						     "--no-drafts")))
	(if (not cobalt--serve-process)
	    (message "Error in running: cobalt serve")
	  (message "Serve process is now running."))))))

(defun cobalt-build (arg)
  "Builds the current site.
Specify a prefix argument (c-u) as ARG to include drafts."
  (interactive "P")
  (when (cobalt--executable-exists-p)
    (let ((default-directory cobalt--current-site))
      (call-process (executable-find "cobalt")
		    nil
		    cobalt-log-buffer-name
		    nil
		    "build"
		    (if (equal arg '(4))
			"--drafts"
		      "--no-drafts")))))

(defun cobalt-serve-kill ()
  "Kill the cobalt serve process, if existing."
  (interactive)
  (when (cobalt--executable-exists-p)
    (when cobalt--serve-process
      (kill-process cobalt--serve-process))
    (setq cobalt--serve-process nil)))

(defun cobalt-new-post (post-title)
  "Ask for POST-TITLE and create a new post."
  (interactive "sWhat is the title of the post? ")
  (cobalt--new-post-with-title post-title t))

(defun cobalt-preview-current-post ()
  "Opens the current buffer."
  (interactive)
  (when (cobalt--executable-exists-p)
    (if (not cobalt--serve-process)
	(message "No serve process is currently running! Call cobalt-serve first!")
      (let* ((post-path (concat (car (butlast (split-string (buffer-name) "\\."))) ".html"))
	     (full-url (concat "http://127.0.0.1:3000/posts/" post-path)))
	(message "Previewing post: %s" full-url)
	(browse-url full-url)))))

(defun cobalt-preview-site ()
  "Preview the site."
  (interactive)
  (when (cobalt--executable-exists-p)
    (if (not cobalt--serve-process)
	(message "No serve process is currently running! Call cobalt-serve first!")
      (browse-url "http://127.0.0.1:3000"))))


(defun cobalt--new-post-with-title (post-title open-file-on-success)
  "Create a new post with POST-TITLE.
Specify OPEN-FILE-ON-SUCCESS if you want to open the file in a buffer if successful."
  (when (cobalt--executable-exists-p)
    (when (not cobalt--current-site)
      (cobalt-change-current-site))
    (let ((default-directory cobalt--current-site)
	  (posts-directory "posts/")
	  (post-file-name (cobalt--convert-title-to-file-name post-title)))
      (apply 'call-process (executable-find "cobalt") nil cobalt-log-buffer-name nil (list "new" "-f" posts-directory post-title))
      (when open-file-on-success
	(if (not (file-exists-p (concat default-directory posts-directory post-file-name ".md")))
	    (message "Could not find file: %s." (concat default-directory posts-directory post-file-name ".md"))
	  (find-file (concat default-directory posts-directory post-file-name ".md")))))))

(defun cobalt--executable-exists-p ()
  "Check if cobalt is installed.  Otherwise it prints a message."
  (if (executable-find "cobalt")
      t
    (message "Cobalt cannot be found in the system.")
    nil))

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
