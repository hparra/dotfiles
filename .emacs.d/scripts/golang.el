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

;; go-autocomplete
(require 'go-autocomplete)
(require 'auto-complete-config)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)


