;;emacs

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

;; Add line-numbers in all programming modes
(add-hook 'prog-mode-hook 'linum-mode)

;; magit
;; http://magit.vc/
(require 'magit)

;; Python
(elpy-enable)
(setq elpy-test-django-with-manage t)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("4904daa168519536b08ca4655d798ca0fb50d3545e6244cefcf7d0c7b338af7e" default)))
 '(flymake-gui-warnings-enabled nil)
 '(package-selected-packages
   (quote
    (multiple-cursors jedi-direx elpy window-numbering markdown-mode magit dracula-theme atom-one-dark-theme arjen-grey-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
