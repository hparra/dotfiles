;;; Golang --- Go langauge configs

;;; Commentary:

;;; Code:

;; eldoc for go
(package-install 'go-eldoc)
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; direx for go
(package-install 'go-direx)
(require 'go-direx)

;; guru
(package-install 'go-guru)
(require 'go-guru)
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

;; https://github.com/nsf/gocode

;; go-autocomplete
(package-install 'auto-complete)				; TODO: Move me
(package-install 'go-autocomplete)
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;;; golang.el ends here
