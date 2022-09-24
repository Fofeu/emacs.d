;;; -*- lexical-binding: t -*-
;; Configure MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "jcs-elpa" (concat proto "://jcs-emacs.github.io/jcs-elpa/packages/")) t)
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
;; Disabled, rarely used currently
;; (when (and (daemonp) (locate-library "edit-server"))
;;   (setq edit-server-new-frame nil)
;;   (edit-server-start))

;; Theme configuration
(defvar theme 'tangotango "The current theme")
(defun ffort-theme-configure-theme ()
  "Loads the theme using the variable theme"
  (load-theme theme t))

;; Allow to restore window configurations
;; Disabled for now
;; (winner-mode 1)

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
  (kill-buffer))

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
(require 'f)
(defcustom ffort-sync-last-filename
  "last-sync"
  "Path to the file storing the date of the last sync"
  :type 'string)

(defun ffort-sync-last-path ()
  (expand-file-name (concat "./" ffort-sync-last-filename) user-emacs-directory))

(defun ffort-sync-write-last ()
  (f-write-text (format "%s" (current-time)) 'utf-8 (ffort-sync-last-path)))

(defun ffort-sync-read-last ()
  (condition-case nil
      (time-convert (read (f-read-text (ffort-sync-last-path))))
      (error '(0 0))))

(defun ffort-sync ()
  (interactive)
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (let ((archive-pkg (car (cdr (assoc pkg package-archive-contents)))))
      (cond
       ((null archive-pkg)
        (lwarn :warning "Package %s is not available in the archive" pkg))

       ((package-desc-p archive-pkg)
        (let ((local-pkg (car (cdr (assoc pkg package-alist))))
              (pkg-name (package-desc-full-name archive-pkg)))
          (cond
           ((null local-pkg) (package-install pkg))
           ((version-list-< (package-desc-version local-pkg)
                            (package-desc-version archive-pkg))
            (package-reinstall pkg))
           ((version-list-= (package-desc-version local-pkg)
                            (package-desc-version archive-pkg))
            (lwarn :debug "Package %s is up-to-date" pkg-name))
           (t (lwarn :debug "Package %s is neither up-to-date nor too old" pkg-name))
           )
          )
        )
       (t (message "%s is not a package but a %s" pkg (symbol-name (type-of pkg))))
       )
      )))

(defun ffort-sync-trigger ()
  (require 'time-date)
  (when (/= (time-to-day-in-year (current-time)) (time-to-day-in-year (ffort-sync-read-last)))
    (ffort-sync)
    (ffort-sync-write-last)
    ))

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
(global-set-key (kbd "C-c c l") 'comment-line)
(global-set-key (kbd "C-c c u") 'uncomment-region)
(global-set-key (kbd "C-c c c") 'open-config)
(global-set-key (kbd "C-c 4 c") 'open-config-other-window)
;;(global-set-key (kbd "C-c l") 'revert-buffer-no-confirm)
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
(global-set-key (kbd "<insert>") nil)

;; Disable commands
;; scroll-left
(global-set-key (kbd "<C-next>") nil)
(global-set-key (kbd "C-x <") nil)
(global-set-key (kbd "C-t") nil)
(global-set-key (kbd "<menu>") nil)

;;compile file of optimization (?)
;;(defun byte-compile-if-newer-and-load (file)
;;   "Byte compile file.el if newer than file.elc"
;;    (if (file-newer-than-file-p (concat file ".el")
;;      (concat file ".elc"))
;;    (byte-compile-file (concat file ".el")))
;;    (load file))
;;(byte-compile-if-newer-and-load "~/.emacs.d/init")

;; Language settings
;; Language Server Protocol (LSP)
;; (add-hook 'prog-mode-hook #'lsp-mode)
;; (add-hook 'prog-mode-hook #'lsp)
(add-hook 'ocaml-mode-hook #'lsp)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; Ocaml
(defun set-merlin-keys ()
  "Set merlin keys"
  (progn
    (define-key merlin-mode-map (kbd "C-c &") nil)
    (define-key merlin-mode-map (kbd "C-c p") 'merlin-pop-stack)))

(defun setup-merlin ()
  (let ((opam-bin (ignore-errors (car (process-lines "opam" "var" "bin")))))
    (when (and opam-bin (file-directory-p opam-bin))
      (add-to-list 'exec-path opam-bin)
      (require 'ocp-indent)
      (autoload 'merlin-mode "merlin" nil t nil)
      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)
      (require 'merlin-company)
      (add-hook 'merlin-mode-hook 'company-mode t)
      (add-hook 'merlin-mode-hook 'set-merlin-keys t))))

;; C/C++
;;(add-hook 'c++-mode-hook 'company-mode)
(defun setup-ccls () (require 'ccls))
;;(add-hook 'c-mode-hook 'company-mode)
;;(add-hook 'c-mode-hook 'hide-ifdef-mode)
;;(add-hook 'hide-ifdef-mode-hook 'hide-ifdefs)

;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (untabify (point-min) (point-max))))
;(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
;(add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
;(add-hook 'focus-in-hook 'auto-virtualenv-set-virtualenv)

;; Prelude
(setq auto-mode-alist (cons '("\\.plu$" . prelude-mode) auto-mode-alist))
(add-to-list 'exec-path "~/site/prelude/bin")
(autoload 'prelude-mode "prelude" "Edition de code prelude" t)

;; LaTeX
(unless (image-type-available-p 'xpm)
  (setq LaTeX-enable-toolbar nil))

(defun latex-config-zathura ()
  "Configure LaTeX mode to support zathura"
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura")))

(defun setup-lsp-ltex ()
  ;; (use-package lsp-ltex
	;;        :ensure t
	;;        :hook (text-mode . (lambda ()
	;; 			    (require 'lsp-ltex)
	;; 			    (lsp))))  ; or lsp-deferred)
  (require 'lsp-ltex)
  (lsp)
  )
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
  "%%% Local"" Variables:\n"
  "%%% mode: latex\n"
  "%%% TeX-command-extra-options: \"-shell-escape\"\n"
  "%%% TeX-master: t\n"
  "%%% End:")
(defun latex-skeleton-add-key()
  (local-set-key (kbd "C-c s") 'latex-skeleton))

(add-hook 'LaTeX-mode-hook 'latex-skeleton-add-key)
(add-hook 'LaTeX-mode-hook 'latex-config-zathura)
(add-hook 'LaTeX-mode-hook 'reftex-mode)

;; HTML
(defun render-html-inplace ()
  (interactive)
  (let (
        (srcbuf (current-buffer))
        (dstbuf (get-buffer-create "*html-render*")))
    (message "srcbuf %s dstbuf %s" srcbuf dstbuf)
    (with-current-buffer dstbuf
      (erase-buffer)
      (shr-insert-document
       (with-current-buffer srcbuf
         (libxml-parse-html-region (point-min) (point-max))))
      )
    ;;(switch-to-buffer dstbuf)
    )
  )

(add-hook 'html-mode-hook 'render-html-inplace)

;; Haskell (Agda too)
(let ((haskell-bin-path (concat (getenv "HOME") "/.cabal/bin")))
  (when (file-directory-p haskell-bin-path)
    (add-to-list 'exec-path haskell-bin-path)))

(defun setup-compilation-advice ()
  (require 'noflet)

  (defadvice compilation-start
      (around inhibit-display
              (command &optional mode name-function highlight-regexp))
    (noflet ((display-buffer (buffer-or-name &optional action frame))
             (set-window-point (window pos))
             (goto-char (position)))
	    (fset 'display-buffer 'ignore)
	    (fset 'goto-char 'ignore)
	    (fset 'set-window-point 'ignore)
	    (save-window-excursion
	      ad-do-it)))

  (ad-activate 'compilation-start))

;; Windows specific
(when (or (eql system-type 'windows-nt))
  ;; Default to Unix file endings
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq-default default-buffer-file-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)

  ;; Font
  (w32-find-non-USB-fonts)
  (add-to-list 'default-frame-alist
             '(font . "Consolas-11"))

  )

;; Hooks
(add-hook 'after-init-hook 'ffort-sync-trigger 0)
(add-hook 'after-init-hook 'ffort-theme-configure-theme 10)
(add-hook 'after-init-hook 'setup-merlin 20)
(add-hook 'after-init-hook 'setup-ccls 20)
(add-hook 'after-init-hook 'setup-lsp-ltex 20)
(add-hook 'after-init-hook 'setup-compilation-advice 20)


;; Custom variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   '(("Make" "make" TeX-run-compile nil t)
     ("TeX" "%(PDF)%(tex) %(file-line-error) %`%(extraopts) %S%(PDFout)%(mode)%' %(output-dir) %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %T" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %(o-dir) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) %(o-dir) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %`%(extraopts) %S%(mode)%' %(output-dir) %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "%(bibtex) %s" TeX-run-BibTeX nil
      (plain-tex-mode latex-mode doctex-mode context-mode texinfo-mode ams-tex-mode)
      :help "Run BibTeX")
     ("Biber" "biber %(output-dir) %s" TeX-run-Biber nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run Biber")
     ("Texindex" "texindex %s.??" TeX-run-command nil
      (texinfo-mode)
      :help "Run Texindex")
     ("Texi2dvi" "%(PDF)texi2dvi %t" TeX-run-command nil
      (texinfo-mode)
      :help "Run Texi2dvi or Texi2pdf")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Convert DVI file to PostScript")
     ("Dvipdfmx" "dvipdfmx -o %(O?pdf) %d" TeX-run-dvipdfmx nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Convert DVI file to PDF with dvipdfmx")
     ("Ps2pdf" "ps2pdf %f %(O?pdf)" TeX-run-ps2pdf nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Convert PostScript file to PDF")
     ("Glossaries" "makeglossaries %(d-dir) %s" TeX-run-command nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run makeglossaries to create glossary file")
     ("Index" "%(makeindex) %s" TeX-run-index nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run makeindex to create index file")
     ("upMendex" "upmendex %(O?idx)" TeX-run-index t
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run upmendex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil
      (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))
 '(TeX-master nil)
 '(auto-save-default nil)
 '(before-save-hook '(delete-trailing-whitespace))
 '(c-basic-offset 2)
 '(c-default-style
   '((c-mode . "bsd")
     (c++-mode . "bsd")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "bsd")))
 '(ccls-executable "/usr/bin/ccls")
 '(column-number-mode t)
 '(company-minimum-prefix-length 1)
 '(delete-selection-mode t)
 '(ffort-last-sync "last-sync")
 '(global-auto-revert-mode t)
 '(global-linum-mode t)
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'text-mode)
 '(initial-scratch-message "")
 '(lsp-keymap-prefix "C-c l")
 '(lsp-ltex-version "15.2.0-linux-x64")
 '(lua-indent-level 2)
 '(make-backup-files nil)
 '(mouse-autoselect-window t)
 '(next-line-add-newlines nil)
 '(org-adapt-indentation t)
 '(org-export-show-temporary-export-buffer nil)
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.3 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-latex-packages-alist '(("" "dsfont" t)))
 '(org-log-done :time)
 '(org-preview-latex-image-directory "/tmp/ltximg/")
 '(org-support-shift-select 'always)
 '(package-selected-packages
   '(proof-general github-tags lsp-ltex noflet gnuplot-mode cuda-mode merlin-company bison-mode auto-virtualenv undo-tree z3-mode cmake-mode cargo lsp-ui ccls yasnippet buttons graphviz-dot-mode tangotango-theme presentation csv-mode json-mode unfill merlin ocp-indent buffer-move auctex edit-server-htmlize edit-server tuareg projectile fold-this company yaml-mode smart-tab lua-mode browse-kill-ring))
 '(python-indent-offset 2)
 '(safe-local-variable-values
   '((TeX-command-extra-options . "-shell-escape")
     (TeX-master . "main")))
 '(show-paren-mode t)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(xterm-mouse-mode t)
 '(z3-solver-cmd "/usr/bin/z3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
