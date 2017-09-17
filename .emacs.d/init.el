;; dotfiles/emacs.d/init.el

;; bind UNIX C-h as delete char
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-?") 'help-command)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;; bind M-h as delete word
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-?") 'mark-paragraph)

;; hide menu, tool, and scroll bar
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-screen t)

;; Enable mouse support
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(put 'set-goal-column 'disabled nil)

;; set tab width
(setq tab-width 2)

;; disable word wrap
(setq-default truncate-lines 1)

;; visualize whitespace
(setq-default show-trailing-whitespace t)
(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))


;; add line-numbers in all programming modes
(add-hook 'prog-mode-hook 'linum-mode)

;; MELPA
(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; Load theme
(load-theme 'atom-one-dark t)

;; magit
;; http://magit.vc/
(require 'magit)

;; Multiple cursors

(require 'multiple-cursors)
(global-set-key (kbd "s-d") 'mc/mark-next-like-this) ; like Atom
;;(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;(global-set-key (kbd "S-s-g") 'mc/mark-all-like-this)

;; like Atom
(global-set-key (kbd "s-\\") 'neotree-toggle)

;;
;; JSON
;;

;; Enable json linting
;; pip install demjson
(require 'flycheck-demjsonlint)

;; Enable flycheck for json
(add-hook 'json-mode-hook #'flycheck-mode)

;;
;; Python
;;

(elpy-enable)
(setq elpy-test-django-with-manage t)

;;
;; Thrift
;;

;; Set Thrift file indent to 4 (whoops)
(defvar thrift-indent-level 4)

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

;; Write custom-set-variable and custom-set-faces elsewhere
;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

