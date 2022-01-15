;;; Configuration for the Dylan LSP server.

(require 'lsp-mode)

(setq lsp-log-io t)
(setq lsp-enable-snippet nil)
(setq lsp-server-trace "verbose")

(defvar dylan-lsp-exe-pathname "dylan-lsp-server"
  "Name of the dylan-lsp-server executable. Must be an absolute pathname
   or the binary must be on your PATH.")

(defvar dylan-lsp-debug-server t
  "If true, the --debug-server option is passed to dylan-lsp-server, which
   causes extra debug output from dylan-lsp-server in the *dylan-lsp* buffer.")

(defvar dylan-lsp-debug-opendylan t
  "If true, the --debug-opendylan option is passed to dylan-lsp-server, which
   causes extra debug output from Open Dylan in the *dylan-lsp* buffer.")

(defvar dylan-lsp-log-pathname nil
  "Pathname of the server's log file. The default is dylan-lsp-server.log, in
   the server's working directory, which is normally the directory of the
   Dylan source file where the LSP client was started.")

(add-to-list 'lsp-language-id-configuration '(dylan-mode . "dylan"))

(defun dylan-lsp-start ()
  (let* ((full-path dylan-lsp-exe-pathname)
         (server (list full-path)))
    (when dylan-lsp-debug-server
      (setq server (append server '("--debug-server"))))
    (when dylan-lsp-debug-opendylan
      (setq server (append server '("--debug-opendylan"))))
    (when dylan-lsp-log-pathname
      (setq server (append server (list "--log" dylan-lsp-log-pathname))))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection server)
                      :major-modes '(dylan-mode)
                      :server-id 'dylan-lsp))))

(dylan-lsp-start)
