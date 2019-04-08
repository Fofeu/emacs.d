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


;; Set load path
;; Add ~/.emacs.d/site-lisp
(add-to-list 'load-path
             (concat user-emacs-directory "/site-lisp"))

;; Launch edit-server in daemon mode
(when (and (daemonp) (locate-library "edit-server"))
  (setq edit-server-new-frame nil)
  (edit-server-start))

;; Load libs
(require 'browse-kill-ring)
(autoload 'org-mode "org")

;; Load theme
(load-theme 'klere t)

;; Do not implicitly add newlines
(setq next-line-add-newlines nil)

;; No backup files plz
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Do not show the startup-screen
(setq inhibit-startup-screen t)

;; Scratch-pad has no initial text + fundamental-mode
(setq initial-scratch-message "")
(setq initial-major-mode 'fundamental-mode)

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

;; Hide toolbar
(tool-bar-mode -1)

;; Cut trailling whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Delete selection when typing
(delete-selection-mode 1)

;; Mouse support in terminal
(xterm-mouse-mode 1)

;; Reload buffer when they have been modified on disk
(global-auto-revert-mode t)

;; Allow to restore window configurations
(winner-mode 1)

(defun reload-config ()
  "Reload configuration file"
  (interactive)
  (load-file user-init-file))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation"
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun kill-current-buffer ()
  "Kill current buffer, if it is unmodified"
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (message "Cannot kill scratch buffer")
    (kill-buffer)))

(defun open-config ()
  "Open the emacs configuration file"
  (interactive)
  (find-file user-init-file))

(defun open-config-other-window ()
  "Open the emacs configuration file"
  (interactive)
  (find-file-other-window user-init-file))

(defun show-current-filename ()
  "Print the current buffer's filename"
  (interactive)
  (message (buffer-file-name)))

(defun split-4-ways ()
  "Split the window in four. Cursor remains in the top left window. Only works if there is only one window open."
  (interactive)
  (when (= 1 (length (window-list)))
    (split-window-right)
    (split-window-below)
    (other-window 2)
    (split-window-below)
    (other-window 2)))

;; Sync packages
(defun sync-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (package-install pkg)))

;; User keys
(global-set-key (kbd "C-c r") 'reload-config)
(global-set-key (kbd "C-c c") 'open-config)
(global-set-key (kbd "C-c 4 c") 'open-config-other-window)
(global-set-key (kbd "C-c l") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-c k") 'kill-current-buffer)
(global-set-key (kbd "C-c y") 'browse-kill-ring)
(global-set-key (kbd "C-c s") 'split-4-ways)
(global-set-key (kbd "C-c w") 'show-current-filename)
(global-set-key (kbd "C-c f") 'fold-this)
(global-set-key (kbd "C-c u") 'fold-this-unfold-at-point)

;; Post-init hook
(add-hook 'after-init-hook 'sync-packages)

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
(defun set-merlin-keys ()
  "Set merlin keys"
  (progn
    (define-key merlin-mode-map (kbd "C-c &") nil)
    (define-key merlin-mode-map (kbd "C-c p") 'merlin-pop-stack)))

(require 'ocp-indent)
(autoload 'merlin-mode "merlin" nil t nil)
(autoload 'merlin-company-backend "merlin" nil t nil)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))
(add-hook 'merlin-mode-hook 'company-mode t)
(add-hook 'merlin-mode-hook 'set-merlin-keys t)

;; C/C++
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(setq c-default-style "bsd" c-basic-offset 2)
(add-hook 'c-mode-hook 'hide-ifdef-mode)
(add-hook 'hide-ifdef-mode-hook 'hide-ifdefs)

;; Lua
(add-hook 'lua-mode-hook
	  (lambda()
	    (setq-default lua-indent-level 2)))

;; Pyton
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq python-indent 2)
	  (untabify (point-min) (point-max))))


;; Prelude
(setq auto-mode-alist (cons '("\\.plu$" . prelude-mode) auto-mode-alist))
(autoload 'prelude-mode "prelude" "Edition de code prelude" t)

;; Org-mode
(add-hook 'orgstruct-mode-hook
          (lambda ()
            (setq-default org-log-done :time))
          t)
(setq org-support-shift-select t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (buffer-move auctex iasm-mode edit-server-htmlize edit-server merlin tuareg projectile fold-this company-irony-c-headers company-irony klere-theme company flycheck-irony yaml-mode smart-tab irony lua-mode browse-kill-ring go-mode)))
 '(safe-local-variable-values
   (quote
    ((TeX-command-extra-options . "-shell-escape")
     (TeX-master . "main")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
