;;; init.el --- tonyfettes' Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This is tonyfettes' Emacs configuration

;;; Code:

;; Use dedicate file for custom.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

;; Don't put ~ file near the source.
(setq backup-directory-alist
      '(("." . (locate-user-emacs-file "backup"))))

(setq backup-by-copying t)

;; Don't put #.# auto save files near the sources.
(setq auto-save-file-name-transforms
      `((".*" ,(locate-user-emacs-file "auto-saves/") t)))

;; Add "lisp" to load-path
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;; Set authenticaion source
(setq auth-sources `(,(locate-user-emacs-file "auth-info")))

;; Don't show scroll bar
(scroll-bar-mode 0)

;; Don't show tool bar
(tool-bar-mode -1)

;; Don't "jump" when move to edges of the screen. See also:
;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq scroll-margin 1
      scroll-conservatively 101
      scroll-preserve-screen-position 'always)

;; Loads frame settings. This is natively different on different
;; platforms. Therefore the content is offloaded to separate file.
(require 'init-frame)

;; Minimize show fringe.
;;
;; Instead setting the width of the fringe to 0, we set its width to be 1.
;; This is to prevent Emacs from displaying annoying '$' symbol at the
;; end/begin of a line when the line is truncated.
;;(fringe-mode 1)

;; Remove the fringe indicator for line truncation.
(setq-default fringe-indicator-alist '())

;; Disable cursor blinking
(blink-cursor-mode 0)

;; Don't wrap line
(set-default 'truncate-lines t)

;; Highlight current cursorline
(global-hl-line-mode)

;; Use space for all indentation
(setq-default indent-tabs-mode nil)

;; Set default tab width to be 2.
(setq-default tab-width 2)

;; Provide `:straight t` in every call to `use-package`.
(defvar straight-use-package-by-default t)

;; Use straight.el as package manager.
;; See https://github.com/radian-software/straight.el
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

(use-package exec-path-from-shell
  :init
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package restart-emacs)

;; Hide minor mode in mode line.
(use-package delight)

(use-package autorevert
  :straight (:type built-in)
  :delight auto-revert-mode)

(use-package eldoc
  :straight (:type built-in)
  :delight)

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
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  ;; Render checkbox when export to HTML
  (setq org-html-checkbox-type 'html)

  (defadvice org-html-paragraph (before org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents))))

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
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; Take screenshot of code
(use-package screenshot)

(use-package tramp)

;; Vundo (undo tree)
(use-package vundo)

;; Git intergration
(use-package magit
  :config
  (setq magit-section-visibility-indicator '("…" . t)))

;; GitHub/GitLab intergration
(use-package forge
  :after magit
  :config
  (defclass forge-gitlab-http-repository (forge-gitlab-repository)
    ((issues-url-format         :initform "http://%h/%o/%n/issues")
     (issue-url-format          :initform "http://%h/%o/%n/issues/%i")
     (issue-post-url-format     :initform "http://%h/%o/%n/issues/%i#note_%I")
     (pullreqs-url-format       :initform "http://%h/%o/%n/merge_requests")
     (pullreq-url-format        :initform "http://%h/%o/%n/merge_requests/%i")
     (pullreq-post-url-format   :initform "http://%h/%o/%n/merge_requests/%i#note_%I")
     (commit-url-format         :initform "http://%h/%o/%n/commit/%r")
     (branch-url-format         :initform "http://%h/%o/%n/commits/%r")
     (remote-url-format         :initform "http://%h/%o/%n")
     (create-issue-url-format   :initform "http://%h/%o/%n/issues/new")
     (create-pullreq-url-format :initform "http://%h/%o/%n/merge_requests/new")
     (pullreq-refspec :initform "+refs/merge-requests/*/head:refs/pullreqs/*")))

  (add-to-list 'ghub-insecure-hosts "192.168.86.1/api/v4"))

;; Diff highlight
(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (diff-hl-flydiff-mode))
  ;; (diff-hl-margin-mode)
  ;; (setq diff-hl-margin-symbols-alist
  ;;       '((insert . "▎")
  ;;         (delete . "▔")
  ;;         (change . "▎")
  ;;         (unknown . "?")
  ;;         (ignored . "i"))))

;; Terminal
(use-package vterm)

;; Which-keys
(use-package which-key
  :delight
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

;; LSP-integration
(use-package eglot
  :hook ((rust-mode
          c++-mode
          latex-mode
          python-mode
          reason-mode
          tuareg-mode) . eglot-ensure))

;; Tree-sitter
(use-package treesit
  :straight (:type built-in)
  :config
  (setq treesit-language-source-alist
        '((moonbit "https://github.com/bzy-debug/tree-sitter-moonbit")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
          (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml"))))

;; Flymake
(use-package flymake
  :straight (:type built-in)
  :config
  (setq flymake-fringe-indicator-position nil))

;; Flycheck
(use-package flycheck
  :ensure t
  :after (flycheck eglot)
  :init (global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'nil))

(use-package flycheck-eglot
  :config
  (global-flycheck-eglot-mode 1))

;; Hook to enable Corfu in minibuffer.
(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point` is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    (setq-local corfu-echo-delay nil
                corfu-popupinfo-mode nil)
    (corfu-mode 1)))

;; At point completion framework
(use-package corfu
  ;; Enable auto completion
  :custom
  (corfu-cycle t)
  (corfu-auto t)

  :bind
  (:map corfu-map
	("RET" . nil)
	([move-beginning-of-line] . nil)
	([move-end-of-line] . nil)
        ("M-SPC" . corfu-insert-separator))

  :init
  (global-corfu-mode)
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

;; Add documentation to minibuffer item.
(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

;; Code actions, but not restricted to language servers.
(use-package embark
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Completion sources
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

;; Show ElDoc in childframe
(use-package eldoc-box
  :config (eldoc-box-hover-at-point-mode))

;; Hexical mode
(use-package nhexl-mode)

;; Rust
(use-package rust-mode)

;; Zig
(use-package zig-mode)

;; OCaml
(use-package tuareg)
(use-package opam-switch-mode)

;; Reason
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
(use-package proof-general
  :config
  ;; Ask opam for path to coqtop. Sould work across platform.
  (setq coq-prog-name (car (process-lines "opam" "exec" "--switch=coq" "which" "coqtop"))))

;; company-coq
(use-package company-coq
  :delight company-coq-mode
  :config
  (setq company-coq-disabled-features '(spinner company company-defaults))
  (setq completion-at-point-functions
        (mapcar #'cape-company-to-capf
                (list #'company-coq-master-backend
                      #'company-coq-choices-backend
                      #'company-math-symbols-latex
                      #'company-math-symbols-unicode)))
  :hook (coq-mode . company-coq-mode))

(use-package js
  :straight (:type built-in)
  :config
  (setq js-indent-level 2))

(use-package css-mode
  :straight (:type built-in)
  :config
  (setq css-indent-offset 2))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(use-package electric
  :straight (:type built-in)
  :config
  (setq electric-indent-inhibit t))

(server-start)

(provide 'init)

;;; init.el ends here
