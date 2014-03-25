;;; VC Backend for OBS(Open Build Service)
;;; By Carl Xiong (xiongc05@gmail.com)
;;;
;;; GPL v3+
;;; 
;;; TODO read meta data about a obs repo, set the api interface accordingly
;;; TODO Info/Geturl Page
;;; TODO Satus page 

(eval-when-compile
  (require 'cl-lib)
  (require 'vc)
  (require 'vc-dir)
  (require 'grep))

(require 's)

;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file. Also see `vc-obs-reload'
(put 'OBS 'vc-functions nil)


(defgroup vc-obs nil
  "VC OBS backend."
  :version "24.1"
  :group 'vc)

(defcustom vc-obs-program "osc"
  "Name of the osc executable (excluding any arguments)."
  :version "24.1"
  :type 'string
  :group 'vc-obs)

(defvar vc-obs-commits-coding-system 'utf-8
  "Default coding system for obs commits.")

;; History of Obs commands.
(defvar vc-obs-history nil)

;;; BACKEND PROPERTIES

(defun vc-obs-revision-granularity () 'repository)
(defun vc-obs-checkout-model (_files) 'implicit)

;;; BACKEND PROPERTIES
(defun vc-obs-registered (file)
  "Check whether FILE is registered with obs."
  (let ((dir (vc-obs-root file)))
    (when dir
      (with-temp-buffer
        (let* (process-file-side-effects
               (name (file-relative-name file dir))
               (str (ignore-errors
                      (cd dir)
                      (vc-obs--out-ok "status" "-v" "--" name)
                      (buffer-string))))
          (and str
               (> (length str) (length name))
               (not (string= (substring str 0 1)
                             "?"))))))))

(defalias 'vc-obs-responsible-p 'vc-obs-root)

(defun vc-obs-state (file)
  "Obs-specific version of `vc-state'."
  (let* ((status (vc-obs--run-command-string
                  file "status" "-v" "--"))
         (status-letter (and status
                             (substring status 0 1))))
    (if status-letter
        (vc-obs--state-code status-letter)
      ;; no status output: up to date
      'up-to-date)))

(defun vc-obs-working-revision (_file)
  "Obs-specific version of `vc-working-revision'.

TODO Only a stub."
  "-1")

(defun vc-obs-diff (files &optional rev1 rev2 buffer)
  "Get a difference report using Obs between two revisions of FILES.

TODO currently it only compares the working copy with last
committed version. We'd like revision support and link diff
capacity."
  (let (process-file-side-effects)
    (apply #'vc-obs-command (or buffer "*vc-diff*") 1 files
	   '("diff"))))

(defun vc-obs-workfile-unchanged-p (file)
  (eq 'up-to-date (vc-obs-state file)))

(defun vc-obs-update-changelog (files)
  "Obs specific implementation of update changelog.

NOTE: FILES are ignored, pre-assumed only one changelog file

TODO Read mail address from other source "
  (let* ((dir (vc-obs-root (buffer-file-name)))
         (changelog-file (let ((default-directory dir))
                           ;; only one file *.changes should be registered this
                           ;; complexity is needed s.t. some temp files will not
                           ;; confuse this function
                           (mapconcat (lambda (f)
                                        (if (vc-obs-registered f)
                                            f
                                          ""))
                                      (file-expand-wildcards "*.changes" t) "")))
         (date-str)
         (header-separator "-------------------------------------------------------------------")
         (mailaddr "cxiong@suse.com")
         (changelog-buffer (find-file-noselect changelog-file)))
    (if (and changelog-buffer
             (buffer-modified-p changelog-buffer))
        ;; user is in the middle of editing changelog, only switch to it
        (progn
          (switch-to-buffer changelog-buffer)
          (message "Continue editing changelog...."))
      ;; insert new entry to changelog
      (setq date-str (s-chomp (with-output-to-string
                                (with-current-buffer standard-output
                                  (apply 'call-process "env" nil '(t nil) nil
                                         '("LC_ALL=POSIX" "TZ=UTC" "date"))))))
      (with-current-buffer changelog-buffer
        (beginning-of-buffer)
        (insert header-separator "\n"
                (format "%s - %s" date-str mailaddr) "\n" "\n"
                "- ")
        (save-excursion
          (insert "\n\n")))
      (switch-to-buffer changelog-buffer)
      (message "Add a new entry to changelog..."))))

(defun vc-obs--call (buffer command &rest args)
  ;; We don't need to care the arguments.  If there is a file name, it
  ;; is always a relative one.  This works also for remote
  ;; directories.  We enable `inhibit-null-byte-detection', otherwise
  ;; Tramp's eol conversion might be confused.
  (let ((inhibit-null-byte-detection t)
	(process-environment (cons "PAGER=" process-environment)))
    (apply 'process-file vc-obs-program nil buffer nil command args)))

(defun vc-obs--out-ok (command &rest args)
  (zerop (apply 'vc-obs--call '(t nil) command args)))

(defun vc-obs--run-command-string (file &rest args)
  "Run a obs command on FILE and return its output as string.
FILE can be nil."
  (let* ((ok t)
         (str (with-output-to-string
                (with-current-buffer standard-output
                  (unless (apply 'vc-obs--out-ok
                                 (if file
				     (append args (list (file-relative-name
							 file)))
				   args))
                    (setq ok nil))))))
    (and ok str)))

(defun vc-obs--state-code (code)
  "Convert from a string to a added/deleted/modified state."
  (pcase (string-to-char code)
    (?\x32 'up-to-date)
    (?A 'added)
    (?C 'conflicted)
    (?D 'deleted)
    (?M 'modified)
    (?? 'new)
    (?! 'missing)))

(defun vc-obs-root (file)
  (or (vc-file-getprop file 'obs-root)
      (vc-file-setprop file 'obs-root (vc-find-root file ".osc"))))


(defun vc-obs-mode-line-string (file)
  "Return a string for `vc-mode-line' to put in the mode line for FILE."
  (let* ((def-ml (vc-default-mode-line-string 'OBS file))
         (help-echo (get-text-property 0 'help-echo def-ml)))
    (propertize def-ml
                'help-echo (concat help-echo "\n:OBS has no branch."))))

(defun vc-obs-revert (file &optional contents-done)
  "Revert FILE to the version stored in the repository."
  (vc-obs-command nil nil file "revert" "--"))

;;; Internal commands
(defun vc-obs-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-obs.el.
The difference to vc-do-command is that this function always invokes
`vc-obs-program'."
  (apply 'vc-do-command (or buffer "*vc*") okstatus vc-obs-program
         file-or-list flags))

;;; Extra helper commands
(defun vc-obs-reload ()
  "Due to the design of vc-hooks, in `vc-call-backend', the first
time a backend function is used, it's recorded in plist of 'OBS
symbol. Usually this doesn't cause any trouble, however if some
backend functions are not defined at the moment of loading, it
might fall back to `vc-default-xxxx' in the plist and wouldn't
include the new functions.

This simple function help development by simply resetting the
plist."
  (interactive)
  (setplist 'OBS nil)
  (message "OBS vc-functions property list is reset."))

(provide 'vc-obs)
