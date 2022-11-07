;;; Configuration for the Dylan LSP server.

(require 'lsp-mode)
(require 'dylan)

(setq lsp-log-io t)
(setq lsp-enable-snippet nil)
(setq lsp-server-trace "verbose")

(defgroup lsp-dylan nil
  "Options controlling the Dylan LSP server."
  :group 'tools
  :prefix "lsp-dylan-")

(defcustom lsp-dylan-exe-pathname "dylan-lsp-server"
  "Name of the dylan-lsp-server executable.
   Must be an absolute pathname or the binary must be on your PATH."
  :type 'string)

(defcustom lsp-dylan-debug-server-flag t
  "Display extra debugging info from the server.
   If true, the --debug-server option is passed to dylan-lsp-server, which
   causes extra debug output from dylan-lsp-server in the *lsp-dylan* buffer.
   This will also cause the server to crash with a backtrace, if a message
   handler encounters an error."
  :type 'boolean)

(defcustom lsp-dylan-debug-opendylan-flag t
  "Display extra debugging info from the compiler.
   If true, the --debug-opendylan option is passed to dylan-lsp-server, which
   causes extra debug output from Open Dylan in the *lsp-dylan* buffer."
  :type 'boolean)

(defcustom lsp-dylan-log-pathname nil
  "Pathname of the server's log file.
   The default is dylan-lsp-server.log, in the server's working directory,
   which is normally the directory of the Dylan source file where the LSP
   client was started."
  :type 'file)

(defcustom lsp-dylan-open-dylan-release nil
  "Absolute pathname of the Open Dylan installation directory.
   If nil, infer the installation directory from the location
   of the dylan-compiler binary, which must be on the path."
  :type '(choice string (const nil)))

(add-to-list 'lsp-language-id-configuration '(dylan-mode . "dylan"))

(defun lsp-dylan--command ()
  "Generate the command line to start the LSP server"
  (append
   (list lsp-dylan-exe-pathname)
   (when lsp-dylan-debug-server-flag '("--debug-server"))
   (when lsp-dylan-debug-opendylan-flag '("--debug-opendylan"))
   (when lsp-dylan-log-pathname (list "--log" lsp-dylan-log-pathname))))

(defun lsp-dylan--infer-install-dir ()
  "Find the install dir relative to `dylan-compiler' on the path"
  (let* ((compiler (or
		    (executable-find "dylan-compiler")
		    (error "Cannot find the Dylan install directory; dylan-compiler must be on the PATH")))
	 (bindir (file-name-directory compiler))
	 (bindirname (directory-file-name bindir))
	 (installdir (file-name-directory bindirname)))
    installdir))

(defun lsp-dylan--environment ()
  "Generate the environment vars to pass to the server."
  ;; Currently OPEN_DYLAN_RELEASE_INSTALL is the only one
  (let ((dotemacs (and (boundp 'lsp-dylan-open-dylan-release)
		       (not (string= "" lsp-dylan-open-dylan-release))
		       lsp-dylan-open-dylan-release))
	(env (getenv "OPEN_DYLAN_RELEASE_INSTALL")))
    (list
     ;; Take value from first of .emacs, the environment, or inferred.
     (cons "OPEN_DYLAN_RELEASE_INSTALL" (or dotemacs
					    env
					    (lsp-dylan--infer-install-dir))))))

(defun lsp-dylan--initialized (workspace)
  "Event handler for when the connection is initialized")

(defun lsp-dylan--start ()
  "Do whatever we need to set up and register with emacs-lsp"
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-dylan--command)
		    :environment-fn 'lsp-dylan--environment
		    :major-modes '(dylan-mode)
		    :initialized-fn 'lsp-dylan--initialized
		    :server-id 'lsp-dylan)))

(lsp-consistency-check lsp-dylan)
(lsp-dylan--start)
