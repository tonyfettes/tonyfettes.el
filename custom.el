;;; custom.el --- tonyfettes' custom file -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex cape cdlatex citar-embark company-coq corfu delight diff-hl
            direnv dirvish dune eldoc-box embark-consult
            exec-path-from-shell flycheck-eglot forge gnuplot god-mode
            image-roll indent-guide marginalia moonbit-mode
            multiple-cursors nhexl-mode ob-sagemath opam-switch-mode
            orderless org-present org-roam pdf-tools proof-general
            pyenv reason-mode restart-emacs rust-mode tablist tuareg
            vertico vundo which-key zig-mode))
 '(package-vc-selected-packages
   '((consult-tramp :url "https://github.com/Ladicle/consult-tramp")
     (moonbit-mode :url "https://github.com/cxa/moonbit-mode")
     (image-roll :url "https://github.com/dalanicolai/image-roll.el")))
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
