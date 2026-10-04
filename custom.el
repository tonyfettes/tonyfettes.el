;;; custom.el --- tonyfettes' custom file -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ee0785c299c1d228ed30cf278aab82cf1fa05a2dc122e425044e758203f097d2"
     default))
 '(package-selected-packages
   '(auctex cape cdlatex citar-embark company-coq consult-tramp copilot
            corfu delight diff-hl direnv dirvish dune eldoc-box
            embark-consult exec-path-from-shell flycheck-eglot forge
            gnuplot gptel image-roll indent-guide marginalia
            moonbit-ts-mode multi-vterm multiple-cursors nhexl-mode
            ob-sagemath opam-switch-mode orderless org-contrib
            org-present org-roam ox-S5 pdf-tools proof-general pyenv
            reason-mode restart-emacs rust-mode tablist tuareg
            vc-use-package vertico vterm vundo which-key zig-mode))
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch
              "main")
     (consult-tramp :url "https://github.com/Ladicle/consult-tramp")
     (image-roll :url "https://github.com/dalanicolai/image-roll.el")))
 '(safe-local-variable-directories
   '("/home/tonyfettes/projects/hazelnut-stepper-data/"
     "/home/tonyfettes/projects/plfa/"))
 '(warning-minimum-level :emergency))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-delete ((t (:inherit nil :foreground "red3"))))
 '(fixed-pitch ((t (:family "Sarasa Mono SC"))))
 '(flycheck-error ((t (:underline "Red1"))))
 '(flycheck-info ((t (:underline "ForestGreen"))))
 '(flycheck-warning ((t (:underline "DarkOrange"))))
 '(fringe ((t (:inherit default))))
 '(indent-guide-face ((t (:foreground "dark gray" :slant normal))))
 '(variable-pitch ((t (:family "Sarasa Gothic SC")))))

(provide 'custom)

;;; custom.el ends here
