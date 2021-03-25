;;; make our server the server for dylan
(require 'lsp-mode)
(setq lsp-print-io t)
(setq lsp-enable-snippet nil)
(setq lsp-server-trace "verbose")

(add-to-list 'lsp-language-id-configuration '(dylan-mode . "dylan"))

(defun lsp-dylan-start ()
  (let* ((relative-path "_build/bin/lsp-dylan")
         (chosen-path
          (cond ((file-exists-p relative-path)
                 ;; If current directory has a _build directory, prefer that.
                 relative-path)
                ((getenv "DYLAN")
                 ;; Assume using dylan-tool and $DYLAN/workspaces/lsp as the
                 ;; workspace directory.
                 ;; TODO(cgay): needs better solution. this works for me.
                 (concat (getenv "DYLAN") "/workspaces/lsp/" relative-path))
                (t
                 (error "Couldn't find the lsp-dylan executable"))))
         (full-path (expand-file-name chosen-path
                                      (file-name-directory (or load-file-name ""))))
         (server (list full-path "--debug-server" "--debug-opendylan")))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection server)
                      :major-modes '(dylan-mode)
                      :server-id 'dylan-lsp))))

(lsp-dylan-start)
