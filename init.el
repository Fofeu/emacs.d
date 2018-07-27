;; Setup lib variables
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-goodies-el")

;; Load libs
(require 'ocp-indent)
(require 'merlin)
(require 'browse-kill-ring)

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

;; set tabwidth
(setq tab-width 2)

;; Cut trailling whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; User keys
(global-set-key (kbd "C-c r") 'reload-config)
;;(global-set-key (kbd "C-c c") 'open-config)
(global-set-key (kbd "C-c l") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-c k") 'kill-if-unmodified)
(global-set-key (kbd "C-c y") 'browse-kill-ring)

;; Ocaml support (?)
;; Tuareg
(load "/home/ffort/.opam/4.06.1/share/emacs/site-lisp/tuareg-site-file")
;; Merlin
;;(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
;;      (when (and opam-share (file-directory-p opam-share))
;;       ;; Register Merlin
;;       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;;       (autoload 'merlin-mode "merlin" nil t nil)
;;       ;; Automatically start it in OCaml buffers
;;       (add-hook 'tuareg-mode-hook 'merlin-mode t)
;;       (add-hook 'caml-mode-hook 'merlin-mode t)
;;       ;; Use opam switch to lookup ocamlmerlin binary
;;       (setq merlin-command 'opam)))

;;compile file of optimization (?)
;;(defun byte-compile-if-newer-and-load (file)
;;   "Byte compile file.el if newer than file.elc"
;;    (if (file-newer-than-file-p (concat file ".el")
;;      (concat file ".elc"))
;;    (byte-compile-file (concat file ".el")))
;;    (load file))
;;(byte-compile-if-newer-and-load "~/.emacs.d/init")

;;;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;;(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;;;; ## end of OPAM user-setup addition for emacs / base ## keep this line
