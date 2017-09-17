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
(custom-theme-set-faces
 'atom-one-dark
 `(col-highlight ((t (:background "#2F343D")))))

;; dired
(setq dired-omit-mode t)			 ; Hide emacs backup and autosave files.
(require 'dired-details)
(setq-default dired-details-hidden-string "")
(dired-details-install)

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

(add-hook 'prog-mode-hook crosshairs-mode)
(setq col-highlight-vline-face-flag t) ; Necessary for col-highlight to work.

;; Font
(setq mac-allow-anti-aliasing t)
(set-face-attribute 'default nil
		    :family "Andale Mono"
		    :weight 'normal)

(setq tab-width 2)											; set tab width
(setq-default truncate-lines 1)					; disable word wrap

;; visualize whitespace
(setq-default show-trailing-whitespace t)
(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

;; add line-numbers in all programming modes
(add-hook 'prog-mode-hook 'linum-mode)

(load "~/.emacs.d/scripts/golang.el")			; Go
(load "~/.emacs.d/scripts/javascript.el")	; JS
(load "~/.emacs.d/scripts/python.el")			; Python
(load "~/.emacs.d/scripts/thrift.el")			; Thrift

;; Write custom-set-variable and custom-set-faces elsewhere
;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

