;;; Configuration for the Dylan LSP server.

(require 'lsp-mode)
(require 'dylan)

(setq lsp-log-io t)
(setq lsp-enable-snippet nil)
(setq lsp-server-trace "verbose")

(defgroup dylan-lsp nil
  "Options controlling the Dylan LSP server."
  :group 'tools
  :prefix "dylan-lsp-")

(defcustom dylan-lsp-exe-pathname "dylan-lsp-server"
  "Name of the dylan-lsp-server executable.
   Must be an absolute pathname or the binary must be on your PATH."
  :type 'string)

(defcustom dylan-lsp-debug-server-flag t
  "Display extra debugging info from the server.
   If true, the --debug-server option is passed to dylan-lsp-server, which
   causes extra debug output from dylan-lsp-server in the *dylan-lsp* buffer.
   This will also cause the server to crash with a backtrace, if a message
   handler encounters an error."
  :type 'boolean)

(defcustom dylan-lsp-debug-opendylan-flag t
  "Display extra debugging info from the compiler.
   If true, the --debug-opendylan option is passed to dylan-lsp-server, which
   causes extra debug output from Open Dylan in the *dylan-lsp* buffer."
  :type 'boolean)

(defcustom dylan-lsp-log-pathname nil
  "Pathname of the server's log file.
   The default is dylan-lsp-server.log, in the server's working directory,
   which is normally the directory of the Dylan source file where the LSP
   client was started."
  :type 'file)

(defcustom dylan-lsp-open-dylan-release nil
  "Absolute pathname of the Open Dylan installation directory.
   If nil, infer the installation directory from the location
   of the dylan-compiler binary, which must be on the path."
  :type '(choice string (const nil)))

(add-to-list 'lsp-language-id-configuration '(dylan-mode . "dylan"))

(defun dylan-lsp--command ()
  "Generate the command line to start the LSP server"
  (append
   (list dylan-lsp-exe-pathname)
   (when dylan-lsp-debug-server-flag '("--debug-server"))
   (when dylan-lsp-debug-opendylan-flag '("--debug-opendylan"))
   (when dylan-lsp-log-pathname (list "--log" dylan-lsp-log-pathname))))

(defun dylan-lsp--infer-install-dir ()
  "Find the install dir relative to `dylan-compiler' on the path"
  (let* ((compiler (or
		    (executable-find "dylan-compiler")
		    (error "Cannot find the Dylan install directory; dylan-compiler must be on the PATH")))
	 (bindir (file-name-directory compiler))
	 (bindirname (directory-file-name bindir))
	 (installdir (file-name-directory bindirname)))
    installdir))

(defun dylan-lsp--environment ()
  "Generate the environment vars to pass to the server."
  ;; Currently OPEN_DYLAN_RELEASE_INSTALL is the only one
  (let ((dotemacs (and (boundp 'dylan-lsp-open-dylan-release)
		       (not (string= "" dylan-lsp-open-dylan-release))
		       dylan-lsp-open-dylan-release))
	(env (getenv "OPEN_DYLAN_RELEASE_INSTALL")))
    (list
     ;; Take value from first of .emacs, the environment, or inferred.
     (cons "OPEN_DYLAN_RELEASE_INSTALL" (or dotemacs
					    env
					    (dylan-lsp--infer-install-dir))))))

(defun dylan-lsp--initialized (workspace)
  "Event handler for when the connection is initialized")

(defun dylan-lsp--start ()
  "Do whatever we need to set up and register with emacs-lsp"
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection 'dylan-lsp--command)
		    :environment-fn 'dylan-lsp--environment
		    :major-modes '(dylan-mode)
		    :initialized-fn 'dylan-lsp--initialized
		    :server-id 'dylan-lsp)))

(lsp-consistency-check dylan-lsp)
(dylan-lsp--start)
