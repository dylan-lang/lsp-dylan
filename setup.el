;;; make our server the server for dylan
(require 'lsp-mode)
(setq lsp-print-io t)
(setq lsp-enable-snippet f)
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (expand-file-name "_build/bin/lsp-dylan"))
                  :major-modes '(dylan-mode)
                  :server-id 'dylan-lsp))
