;;
;; Golang
;;

;; eldoc for go
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; direx for go
(require 'go-direx)

;; guru
(require 'go-guru)
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

;; https://github.com/nsf/gocode

;; go-autocomplete
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)


