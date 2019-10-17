;;; make our server the server for dylan
(require 'lsp-mode)
(setq lsp-print-io t)
(setq lsp-enable-snippet nil)

(add-to-list 'lsp-language-id-configuration '(dylan-mode . "dylan"))
(let ((server (expand-file-name "_build/bin/lsp-dylan"
				(file-name-directory load-file-name))))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection server)
                    :major-modes '(dylan-mode)
                    :server-id 'dylan-lsp)))
