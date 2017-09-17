;;
;; JSON
;;

;; Enable json linting
;; pip install demjson
(require 'flycheck-demjsonlint)

;; Enable flycheck for json
(add-hook 'json-mode-hook #'flycheck-mode)
