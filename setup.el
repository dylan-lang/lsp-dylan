;;; make our server the server for dylan
(require 'lsp-mode)
(setq lsp-print-io t)
(setq lsp-enable-snippet nil)
(setq lsp-server-trace "verbose")

(add-to-list 'lsp-language-id-configuration '(dylan-mode . "dylan"))

(defun lsp-dylan-start ()
  (let* ((dylan-root (getenv "DYLAN"))
         (lsp-dylan-relative-path "_build/bin/lsp-dylan")
         (lsp-dylan-full-path
          (if dylan-root
              ;; Assume using dylan-tool if $DYLAN is set.
              ;; Also assume $DYLAN/workspaces/lsp as the workspace directory.
              (format "%s/workspaces/lsp/_build/bin/lsp-dylan" dylan-root)
            ;; Otherwise assume using git submodules in the lsp-dylan directory.
            (expand-file-name lsp-dylan-relative-path load-file-name)))
         (server (list lsp-dylan-full-path "--debug")))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection server)
                      :major-modes '(dylan-mode)
                      :server-id 'dylan-lsp))))

(lsp-dylan-start)
