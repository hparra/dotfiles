;; dotfiles/emacs.d/init.el

;; bind UNIX C-h as delete char
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-?") 'help-command)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;; bind M-h as delete word
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-?") 'mark-paragraph)

;; hide menu, tool, and scroll bar
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-screen t)

;; Enable mouse support
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(put 'set-goal-column 'disabled nil)

;; Always treat underscore as part of word
(modify-syntax-entry ?_ "w" (standard-syntax-table))

;; Font
(setq mac-allow-anti-aliasing t)
(set-face-attribute 'default nil
		    :family "Andale Mono"
		    :weight 'normal)

(setq default-tab-width 2)		; Tabs look like two spaces.
(setq-default truncate-lines 1)		; disable word wrap

;; visualize whitespace
(setq-default show-trailing-whitespace t)
(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

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
(package-refresh-contents)

;; Fix $PATH
(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "GOPATH")

;; Flycheck
(package-install 'flycheck)
(global-flycheck-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

(package-install 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)

;; Set different settings for GUI versus terminal.
(if (display-graphic-p)			;; if GUI
    (progn
			(load-theme 'atom-one-dark t)
			(custom-theme-set-faces
			 'atom-one-dark
			 `(col-highlight ((t (:background "#2F343D"))))))
	(load-theme 'wombat t))

;; dired
(setq dired-omit-mode t)			 ; Hide emacs backup and autosave files.
(require 'dired-details)
(setq-default dired-details-hidden-string "")
(dired-details-install)
(setq dired-dwim-target t)

;; magit
;; http://magit.vc/
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; diff-hl
(global-diff-hl-mode)
(diff-hl-flydiff-mode 1)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "S-s-g") 'mc/mark-all-like-this) ; like Atom

;; like Atom
(global-set-key (kbd "s-\\") 'neotree-toggle)

;; Highlight current line.
(global-hl-line-mode +1)

;; Crosshairs
;; NOTE: This stopped working sometime ago for unknown reasons
;; (setq col-highlight-vline-face-flag t) ; Necessary for col-highlight to work.
;; (add-hook 'prog-mode-hook crosshairs-mode)

;; Mark lines at location where they pass n-column.
;; (require 'column-marker)
;; (add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))

;; Add line-numbers in all text modes.
(add-hook 'text-mode-hook 'nlinum-mode)

(load "~/.emacs.d/scripts/golang.el")			; Go
(load "~/.emacs.d/scripts/javascript.el")	; JS
(load "~/.emacs.d/scripts/python.el")			; Python
(load "~/.emacs.d/scripts/thrift.el")			; Thrift

;; Write custom-set-variable and custom-set-faces elsewhere
;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

