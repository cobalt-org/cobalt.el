;;; cobalt.el --- Summary   -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;;; Todo:
;; - Create a test for cobalt-publish.
;; - Create function that checks if the current buffer is a valid post.
;; - Get the posts directory from the _cobalt.yml file.
;; - Only preview a buffer if it is a valid post.
;; - If post is a draft, and cobalt-serve was not run with "--drafts", then don't allow previewing.
;; - If start-process returns an error don't let it set cobalt--serve-process
;; - Fix error with cobalt-build when cobalt--current-site is nil.
;; - Add a license file.

(defcustom cobalt-site-paths nil
  "Variable that holds a list of cobalt sites."
  :group 'cobalt
  :type 'sexp)

(defcustom cobalt-log-buffer-name "*cobalt*"
  "Name of the log buffer for cobalt process output."
  :group 'cobalt
  :type 'string)

(defcustom cobalt-serve-port 3000
  "The port to serve the site on."
  :group 'cobalt
  :type 'number)

(defvar cobalt--serve-process nil
  "Use to save cobalt serve process is so it can be killed in the future.")

(defvar cobalt--current-site nil
  "The current site to run cobalt commands on.")

(defun cobalt-command (args)
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

(defun cobalt-init (args)
  "Create a new cobalt site at the given path indicated by ARGS."
  (interactive "DDirectory to create site: ")
  (apply 'call-process
	 (executable-find "cobalt")
	 nil
	 cobalt-log-buffer-name
	 nil
	 (list "init" args)))

(defun cobalt-change-current-site ()
  "Show a selection to switch current site.
Kills an exiting server process.  User should run cobalt-serve again for the newly switch site."
  (interactive)
  (when (cobalt--executable-exists-p)
    (when cobalt--serve-process
      (cobalt-serve-kill)
      (cobalt--log (concat "Server killed for " cobalt--current-site)))
    (if (not (and cobalt-site-paths (> (length cobalt-site-paths) 0) ))
	(cobalt--log "cobalt-site-paths is empty! Set it first.")
      (setq cobalt--current-site (completing-read "Select site to use as current: " cobalt-site-paths nil t))
      (cobalt--log (concat "Current cobalt site set to " cobalt--current-site)))))

(defun cobalt-serve (arg)
  "Build, serve, and watch the project at the source dir.
Specify a prefix argument (c-u) as ARG to include drafts."
  (interactive "P")
  (when (cobalt--executable-exists-p)
    (if cobalt--serve-process
	(cobalt--log "Serve process already running!")
      (when (not cobalt--current-site)
	(cobalt-change-current-site))
      (let* ((default-directory cobalt--current-site))
	(setq cobalt--serve-process (start-process "cobalt-serve"
						   cobalt-log-buffer-name
						   (executable-find "cobalt")
						   "serve"
						   (if (equal arg '(4))
						       "--drafts"
						     "--no-drafts")
						   "--port"
						   (number-to-string cobalt-serve-port)))
	(if (not cobalt--serve-process)
	    (cobalt--log "Error in running: cobalt serve")
	  (cobalt--log (concat "Serve process is now running. "
			       (if (equal arg '(4))
				   "Drafts included."
				 "Drafts NOT included."))))))))

(defun cobalt-serve-kill ()
  "Kill the cobalt serve process, if existing."
  (interactive)
  (when (cobalt--executable-exists-p)
    (when cobalt--serve-process
      (kill-process cobalt--serve-process))
    (setq cobalt--serve-process nil)))

(defun cobalt-preview-site ()
  "Preview the site."
  (interactive)
  (when (cobalt--executable-exists-p)
    (if (not cobalt--serve-process)
	(cobalt--log "No serve process is currently running! Call cobalt-serve first!")
      (browse-url "http://127.0.0.1:3000"))))

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
		      "--no-drafts")))
    (cobalt--log (concat "Site built successfully. "
			(if (equal arg '(4))
			    "Drafts included."
			  "Drafts NOT included.")))))

(defun cobalt-new-post (post-title)
  "Ask for POST-TITLE and create a new post."
  (interactive "sWhat is the title of the post? ")
  (cobalt--new-post-with-title post-title t))

(defun cobalt--new-post-with-title (post-title open-file-on-success)
  "Create a new post with POST-TITLE.
Specify OPEN-FILE-ON-SUCCESS if you want to open the file in a buffer if successful."
  (when (cobalt--executable-exists-p)
    (when (not cobalt--current-site)
      (cobalt-change-current-site))
    (let ((default-directory cobalt--current-site)
	  (posts-directory "posts/")
	  (post-file-name (cobalt--convert-title-to-file-name post-title)))
      (apply 'call-process
	     (executable-find "cobalt")
	     nil
	     cobalt-log-buffer-name
	     nil
	     (list "new" "-f" posts-directory post-title))
      (when open-file-on-success
	(if (not (file-exists-p (concat default-directory posts-directory post-file-name ".md")))
	    (cobalt--log (concat "Could not find file: " default-directory posts-directory post-file-name ".md"))
	  (find-file (concat default-directory posts-directory post-file-name ".md")))))))

(defun cobalt-preview-current-post ()
  "Opens the current post buffer."
  (interactive)
  (when (cobalt--executable-exists-p)
    (if (not cobalt--serve-process)
	(cobalt--log "No serve process is currently running! Call cobalt-serve first!")
      (let* ((post-path (concat (car (butlast (split-string (buffer-name) "\\."))) ".html"))
	     (full-url (concat "http://127.0.0.1:3000/posts/" post-path)))
	(cobalt--log (concat "Previewing post: " full-url))
	(browse-url full-url)))))

(defun cobalt-publish ()
  "Publishes the current post buffer."
  (interactive)
  (when (cobalt--executable-exists-p)
    (apply 'call-process
	   (executable-find "cobalt")
	   nil
	   cobalt-log-buffer-name
	   nil
	   (list "publish" (buffer-name)))
    (cobalt--log (concat "Successfully published the post:" (buffer-name)))
    ))
  

(defun cobalt--executable-exists-p ()
  "Check if cobalt is installed.  Otherwise it prints a message."
  (if (executable-find "cobalt")
      t
    (cobalt--log "Cobalt cannot be found in the system.")
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

(defun cobalt--log (str)
  "Internal logger that logs STR to messages and the cobalt log buffer."
  (message "[cobalt.el] %s" str)
  (let ((log-buffer (get-buffer-create cobalt-log-buffer-name)))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (open-line 1)
      (forward-line 1)
      (insert (concat "[cobalt.el] " str)))))

(provide 'cobalt)
;;; cobalt.el ends here
