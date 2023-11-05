;;; init.el --- tonyfettes's Emacs configuration

;;; Commentary:

;; This is tonyfettes's Emacs configuration.

;;; Code:

;; https://github.com/raxod502/straight.el#getting-started

;; Prevent Emacs from creating backup file
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-directory-alist '(("" . "~/.config/emacs/backup")))

;; Don't "jump" when move to edges of the screen. See also:
;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq scroll-step 1
      scroll-margin 1
      redisplay-dont-pause t
      scroll-conservatively 101
      scroll-preserve-screen-position 1)

;; Close menu/scroll/tool bar
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; Disable cursor blinking
(blink-cursor-mode 0)

;; Set font.
;; (add-to-list 'default-frame-alist
;;        '(font . "monospace-12"))

;; Show line numbers.
;; (global-display-line-numbers-mode)

;; set nowrap
(set-default 'truncate-lines t)

;; Highlight current cursor line
(global-hl-line-mode)

;; Use space for all indentation
(setq-default indent-tabs-mode nil)

;; Turn off the electric indent mode
(electric-indent-mode -1)

;; Use straight.el as package manager.
;; See https://github.com/raxod502/straight.el
;; Provide `:straight t` in every call to `use-package`.
(defvar straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package. See https://github.com/jwiegley/use-package
(straight-use-package 'use-package)

;; Import environment variables from shell.
(use-package exec-path-from-shell
  :config (when (daemonp) (exec-path-from-shell-initialize)))

(use-package restart-emacs)

;; Org mode
(use-package org
  :straight (:type built-in)
  :config
  ;; Startup with indentation
  (setq org-startup-indented t)
  ;; Startup with inline images
  (setq org-startup-with-inline-images t)
  ;; Set default width of image to be 400
  (setq org-image-actual-width 400)
  (setq org-directory '("~/workspace/"))

  ;; Org-agenda settings
  (setq org-agenda-files '("~/workspace/"))
  ;; Don't show overdue items
  (setq org-scheduled-past-days 0)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline)
  ;; Persistent clocking statistics across Emacs sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  ;; Export VTODO items from TODO entries
  (setq org-icalendar-include-todo 'all)
  ;; Start the agenda views from today
  (setq org-agenda-start-on-weekday nil)
  ;; Set path for exported combined iCalendar file
  (setq org-icalendar-combined-agenda-file "~/personal/calendars/org.ics")
  ;; Include breadcrumbs in the org-agenda view
  '((agenda . " %i %-12:c%?-12t% s %b")
    (todo . " %i %-12:c")
    (tags . " %i %-12:c")
    (search . " %i %-12:c"))

  ;; Org-babel Settings
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)))
  ;; Do not confirm before evaluation
  (setq org-confirm-babel-evaluate nil)
  ;; Do not evaluate code blocks when exporting.
  (setq org-export-babel-evaluate nil)
  ;; Display images after evaluating code blocks
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images))

(use-package ox-latex
  :straight (:type built-in)
  :config
  (add-to-list 'org-latex-classes
               '("ctexart"
                 "\\documentclass{ctexart}
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package org-protocol
  :straight (:type built-in))

(use-package cdlatex)

;; Calendar interface
;; (use-package calfw)
;; (use-package calfw-org)
;; (use-package calfw-ical)

;;; I now use org-icalendar-combine-agenda-files for exporting ics and
;;; upload it to radicale server using curl.
;; (use-package org-caldav
;;   :config
;;   (setq org-caldav-url "https://api.tonyfettes.com/radicale")
;;   (setq org-caldav-calendar-id "main")
;;   (setq org-caldav-inbox "~/personal/org/caldav-inbox.org")
;;   (setq org-caldav-files ""))

;; Indent guide
(use-package indent-guide
  :init (indent-guide-global-mode)
  :config (setq indent-guide-char "|"))

;; Undo tree
;; (use-package undo-tree
;;  :config (global-undo-tree-mode t))

;; Git intergration
(use-package magit)

;; Which-keys
(use-package which-key
  :config (which-key-mode))

;; Vertical completion in minibuffer
(use-package vertico
  :init (vertico-mode))

;; Save history of completions (vertico)
(use-package savehist :init (savehist-mode))

(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  
  ;; Corfu
  (setq tab-always-indent 'complete))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Commend-line completion
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; Select tramp using consult
(use-package consult-tramp
  :straight (:host github
             :repo "Ladicle/consult-tramp"))

;; Syntax highlighting
(use-package tree-sitter :init (global-tree-sitter-mode))
(use-package tree-sitter-langs)

;; LSP-integration
(use-package eglot
  :hook ((rust-mode
          c++-mode
          latex-mode) . eglot-ensure))

;; At point completion framework
(use-package corfu
  :custom (corfu-auto t)
  :bind
  (:map corfu-map
	("RET" . nil)
	([remap move-beginning-of-line] . nil)
	([remap move-end-of-line] . nil))
  :init (global-corfu-mode))

;; Completion sources
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

;; Show ElDoc in childframe
(use-package eldoc-box
  :config (eldoc-box-hover-at-point-mode))

;; Email client
;; (use-package mu4e)

;; Hexical mode
(use-package nhexl-mode)

;; Rust
(use-package rust-mode)

;; Zig
(use-package zig-mode)

;; Flex/Bison
(use-package bison-mode)

;; OCaml and Reason
(use-package reason-mode
  :hook (reason-mode . eglot-ensure))

;; LaTeX
(use-package tex
  :straight auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  (setq TeX-view-program-selection
        '((output-dvi "xdvi")
          (output-pdf "Zathura")
          (output-html "xdg-open"))))
;; PDF
(use-package pdf-tools
  :straight (:host github
             :repo "dalanicolai/pdf-tools"
             :branch "pdf-roll"
             :files ("lisp/*.el"
                     "README"
                     ("build" "Makefile")
                     ("build" "server")
                     (:exclude "lisp/tablist.el" "lisp/tablist-filter.el")))
  :requires image-roll)

;; Smoother PDF/image scroll
(use-package image-roll
  :straight (:host github
             :repo "dalanicolai/image-roll.el"))

;; Markdown
(use-package markdown-mode)

;; Sage
(use-package sage-shell-mode)
;; Sage support or Org-babel
(use-package ob-sagemath
  :config
  (setq org-babel-default-header-args:sage
        '((:session . t)
          (:results . "output")))
  ;; Asynchronous evaluation
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async)))

;; gnuplot 
(use-package gnuplot)

;; Z3 SMT-LIB
(use-package z3-mode)

;; Proof General, for Coq
(use-package proof-general)

;; Agda
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; (setq auto-mode-alist
;;       (append
;;        '(("\\.agda\\'" . agda2-mode)
;; 	 ("\\.lagda.md\\'" . agda2-mode))
;;        auto-mode-alist))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-deadcode-face ((t (:foreground "#616e88"))))
 '(agda2-highlight-keyword-face ((t (:foreground "#81A1C1"))))
 '(agda2-highlight-module-face ((t (:foreground "#8FBCBB"))))
 '(agda2-highlight-number-face ((t (:foreground "#B48EAD"))))
 '(agda2-highlight-operator-face ((t (:foreground "#81A1C1"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "#8FBCBB"))))
 '(agda2-highlight-record-face ((t (:foreground "#8FBCBB")))))

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("30feaaa766f262fa91b170aadf727a6dca5e72f14d728e594d2622dd04f27b0b" default))
 '(org-agenda-files
   '("~/workspace/fscut/main.org" "/home/tonyfettes/workspace/fplab/main.org" "/home/tonyfettes/workspace/courses/ve311/main.org" "/home/tonyfettes/workspace/job/main.org" "/home/tonyfettes/workspace/courses/ve441/main.org" "/home/tonyfettes/workspace/courses/ve320/main.org"))
 '(sage-shell:use-prompt-toolkit nil)
 '(sage-shell:use-simple-prompt t))
