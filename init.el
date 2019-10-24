;;; -*- lexical-binding: t -*-
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

;; Load theme
(load-theme 'tangotango t)

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

;; Frame setup
(defun frame-setup (&optional frame)
  "Function callend when frame is configured.
Currently only disables tool-bar in graphical mode."
  (when (display-graphic-p frame)
    (tool-bar-mode -1)))

(mapc 'frame-setup (frame-list))
(add-hook 'after-make-frame-functions 'frame-setup)

;; Cut trailling whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Delete selection when typing
(delete-selection-mode 1)

;; Mouse support in terminal
(xterm-mouse-mode 1)

;; Reload buffer when they have been modified on disk
(global-auto-revert-mode t)

;; Allow to restore window configurations
;; Disabled for now
;; (winner-mode 1)

;; Window focus follows mouse
(setq mouse-autoselect-window t)

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
    (let* ((archive-pkg (car (cdr (assoc pkg package-archive-contents))))
           (archive-version (package-desc-version archive-pkg))
           (local-pkg (car (cdr (assoc pkg package-alist)))))
      (cond
       ((null local-pkg)
        (package-install pkg))
       ((version-list-< (package-desc-version local-pkg) archive-version)
        (package-reinstall pkg))))))

(defun exit-emacs-sensibly ()
  (interactive)
  (if server-mode
      (delete-frame)
    (save-buffers-kill-terminal)))

(defun wc()
  (interactive)
  (shell-command (shell-quote-argument (concat "wc -w " buffer-file-name))))

;; User keys
(global-set-key (kbd "C-c r") 'reload-config)
(global-set-key (kbd "C-c c r") 'comment-region)
(global-set-key (kbd "C-c c u") 'uncomment-region)
(global-set-key (kbd "C-c c c") 'open-config)
(global-set-key (kbd "C-c 4 c") 'open-config-other-window)
(global-set-key (kbd "C-c l") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-c k") 'kill-current-buffer)
(global-set-key (kbd "C-c y") 'browse-kill-ring)
;;(global-set-key (kbd "C-c s") 'mode-skeleton)
(global-set-key (kbd "C-c p") 'show-current-filename)
(global-set-key (kbd "C-c f f") 'fold-this)
(global-set-key (kbd "C-c f u") 'fold-this-unfold-at-point)
(global-set-key (kbd "C-c g") 'goto-line)
;;(global-set-key (kbd "C-c d") 'kill-whole-line)
(global-set-key (kbd "C-c b <left>") 'buf-move-left)
(global-set-key (kbd "C-c b <right>") 'buf-move-right)
(global-set-key (kbd "C-c i b") 'ispell-buffer)
(global-set-key (kbd "C-c i w") 'ispell-word)
(global-set-key (kbd "C-c i d") 'ispell-change-dictionary)
(global-set-key (kbd "C-c w") 'ispell-word)
(global-set-key (kbd "C-c q") 'exit-emacs-sensibly)
(global-set-key (kbd "C-c j f") 'fill-paragraph)
(global-set-key (kbd "C-c j u") 'unfill-paragraph)
(global-set-key (kbd "C-c d b") 'ediff-buffers)

;; Post-init hook
(if (daemonp)
    (add-hook 'after-init-hook 'sync-packages t))

;;compile file of optimization (?)
;;(defun byte-compile-if-newer-and-load (file)
;;   "Byte compile file.el if newer than file.elc"
;;    (if (file-newer-than-file-p (concat file ".el")
;;      (concat file ".elc"))
;;    (byte-compile-file (concat file ".el")))
;;    (load file))
;;(byte-compile-if-newer-and-load "~/.emacs.d/init")

;; Package configurations

(defun company-customization ()
  "Set company customization"
  (setq company-minimum-prefix-length 1))

(add-hook 'company-mode-hook 'company-customization)

;; Language settings

;; Ocaml
(defun set-merlin-keys ()
  "Set merlin keys"
  (progn
    (define-key merlin-mode-map (kbd "C-c &") nil)
    (define-key merlin-mode-map (kbd "C-c p") 'merlin-pop-stack)))

(let ((opam-bin (ignore-errors (car (process-lines "opam" "config" "var" "bin")))))
  (when (and opam-bin (file-directory-p opam-bin))
    (add-to-list 'exec-path opam-bin)
    (require 'ocp-indent)
    (autoload 'merlin-mode "merlin" nil t nil)
    (autoload 'merlin-company-backend "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'merlin-company-backend))
    (add-hook 'merlin-mode-hook 'company-mode t)
    (add-hook 'merlin-mode-hook 'set-merlin-keys t)))

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

;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent 2)
            (untabify (point-min) (point-max))))


;; Prelude
(setq auto-mode-alist (cons '("\\.plu$" . prelude-mode) auto-mode-alist))
(add-to-list 'exec-path "~/site/prelude/bin")
(autoload 'prelude-mode "prelude" "Edition de code prelude" t)

;; Org-mode
(add-hook 'orgstruct-mode-hook
          (lambda ()
            (setq-default org-log-done :time))
          t)
(setq org-support-shift-select t)
(setq org-log-done t)

;; Store latex previews in /tmp
(setq org-preview-latex-image-directory "/tmp/ltximg/")

(defvar org-latex-scale-factor 1.3
  "The scale factor applied to LaTeX-fragments in org-mode")

(defun org-latex-increase-scale()
  "Applies org-latex-scale-factor"
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale org-latex-scale-factor)))
(add-hook 'org-mode-hook 'org-latex-increase-scale)

(defun latex-org-add-packages ()
  "Add packages to compilation of latex-fragments in org-mode"
  (cons '(nil "bussproofs" t) org-latex-packages-alist))
(add-hook 'org-mode-hook 'latex-org-add-packages)

;; Coq
(add-hook 'coq-mode-hook #'company-coq-mode)

;; LaTeX
(setq-default TeX-master nil)

(unless (image-type-available-p 'xpm)
  (setq LaTeX-enable-toolbar nil))

(defun latex-config-zathura ()
  "Configure LaTeX mode to support zathura"
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura")))

(define-skeleton latex-skeleton
  "Inserts a Latex skelleton with average settings"

  "Class: "
  "\\documentclass{" str | "article"  "}\n"
  "\n"
  "\\usepackage{multicol}\n"
  "\\usepackage[OT1]{fontenc}\n"
  "\\usepackage[utf8]{inputenc}\n"
  "\n"
  "\\usepackage{listings}\n"
  "\\usepackage[stable]{footmisc}\n"
  "\\usepackage{hyperref}\n"
  "\\usepackage{amssymb}\n"
  "\\usepackage{amsmath}\n"
  "\\usepackage{rtsched}\n"
  "\\usepackage{multido}\n"
  "\\usepackage{multirow}\n"
  "\\usepackage{makecell}\n"
  "\\usepackage{diagbox}\n"
  "\\usepackage{cite}\n"
  "\n"
  "\\usepackage{minted}\n"
  "\\usemintedstyle{pastie}\n"
  "\n"
  "\\usepackage{tikz}\n"
  "\\usetikzlibrary{positioning}\n"
  "\\usetikzlibrary{calc}\n"
  "\n"
  "\\begin{document}\n"
  _ "\n"
  "\\end{document}\n\n"
  "%%% Local Variables:\n"
  "%%% mode: latex\n"
  "%%% TeX-command-extra-options: \"-shell-escape\"\n"
  "%%% TeX-master: t\n"
  "%%% End:")
(defun latex-skeleton-add-key()
  (local-set-key (kbd "C-c s") 'latex-skeleton))

(add-hook 'LaTeX-mode-hook 'latex-skeleton-add-key)
(add-hook 'LaTeX-mode-hook 'latex-config-zathura)
(add-hook 'LaTeX-mode-hook 'reftex-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (tangotango-theme presentation proof-general company-coq csv-mode json-mode unfill merlin ocp-indent flymake-rust rust-mode buffer-move auctex iasm-mode edit-server-htmlize edit-server tuareg projectile fold-this company-irony-c-headers company-irony company flycheck-irony yaml-mode smart-tab lua-mode browse-kill-ring go-mode)))
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
