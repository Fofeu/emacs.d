;; Configure MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Load libs
(require 'browse-kill-ring)
(autoload 'org-mode "org")

;; Do not implicitly add newlines
(setq next-line-add-newlines 'nil)

;; No backup files plz
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Do not show the startup-screen
(setq inhibit-startup-screen t)

;; Scratch-pad has no initial text
(setq initial-scratch-message "")

;; Highlight matching parenthesis
(show-paren-mode 1)

;; Show line and column numbers
(global-linum-mode t)
(setq column-number-mode t)

;; Prettier line wrap
(global-visual-line-mode 1)

;; set indentation
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Cut trailling whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Delete selection when typing
(delete-selection-mode 1)

;; Mouse support in terminal
(xterm-mouse-mode 1)

(defun reload-config ()
  "Reload configuration file"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation"
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun kill-if-unmodified ()
  "Kill current buffer, if it is unmodified"
  (interactive)
  (if (buffer-modified-p)
      (message "Buffer is kept because it was modified")
    (kill-buffer)))

(defun open-config ()
  "Open the emacs configuration file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; User keys
(global-set-key (kbd "C-c r") 'reload-config)
(global-set-key (kbd "C-c c") 'open-config)
(global-set-key (kbd "C-c l") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-c k") 'kill-if-unmodified)
(global-set-key (kbd "C-c y") 'browse-kill-ring)

;;compile file of optimization (?)
;;(defun byte-compile-if-newer-and-load (file)
;;   "Byte compile file.el if newer than file.elc"
;;    (if (file-newer-than-file-p (concat file ".el")
;;      (concat file ".elc"))
;;    (byte-compile-file (concat file ".el")))
;;    (load file))
;;(byte-compile-if-newer-and-load "~/.emacs.d/init")

;; Language settings

;; Ocaml
(setq opam-cmd-share (shell-command-to-string "opam config var share 2> /dev/null"))
(unless (string= "" opam-cmd-share)
  (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-goodies-el")
  (require 'ocp-indent)
  (autoload 'merlin-mode "merlin")
  (load "~/.opam/4.06.1/share/emacs/site-lisp/tuareg-site-file")
  (add-hook 'tuareg-mode-hook 'merlin-mode))

;; C/C++
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Lua
(add-hook 'lua-mode-hook
	  (lambda()
	    (setq-default lua-indent-level 2)))

;; Pyton
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq python-indent 2)
	  (untabify (point-min) (point-max))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-irony yaml-mode smart-tab irony lua-mode browse-kill-ring))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
