;;; Configuration for the Dylan LSP server.

(require 'lsp-mode)
(require 'dylan)

(setq lsp-log-io t)
(setq lsp-enable-snippet nil)
(setq lsp-server-trace "verbose")

(defgroup lsp-dylan nil
  "Options controlling the Dylan LSP server."
  :group 'lsp-mode
  :prefix "lsp-dylan-"
  :link '(url-link "https://github.com/dylan-lang/lsp-dylan/blob/master/README.md"))


(defcustom lsp-dylan-exe-pathname "dylan-lsp-server"
  "Name of the dylan-lsp-server executable.
   Must be an absolute pathname or the binary must be on your PATH."
  :type 'string)

(defcustom lsp-dylan-extra-command-line-options
  '("--debug-opendylan" "--debug-server")
  "Extra command-line options to pass to dylan-lsp-server.
   See `dylan-lsp-server --help` for available options."
  :type '(repeat string))

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
   lsp-dylan-extra-command-line-options
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
  (let* ((install-dir (or lsp-dylan-open-dylan-release
                          (getenv "OPEN_DYLAN_RELEASE_INSTALL")
                          (lsp-dylan--infer-install-dir)
                          (error "Can't find Open Dylan install directory")))
         (registry-dir (or (getenv "OPEN_DYLAN_USER_REGISTRIES")
                           (expand-file-name "sources/registry"
                                             (file-name-as-directory install-dir)))))
    (list
     ;; Take value from first of .emacs, the environment, or inferred.
     (cons "OPEN_DYLAN_RELEASE_INSTALL" install-dir)
     (cons "OPEN_DYLAN_USER_REGISTRIES" registry-dir))))

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
