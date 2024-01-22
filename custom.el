;;; custom.el --- tonyfettes' custom file -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex cape cdlatex company-coq consult-tramp corfu delight diff-hl
            eldoc-box embark-consult exec-path-from-shell
            flycheck-eglot forge gnuplot hide-mode-line image-roll
            indent-guide marginalia nhexl-mode ob-sagemath
            opam-switch-mode orderless org-contrib org-present
            pdf-tools proof-general reason-mode restart-emacs
            rust-mode tablist tuareg vc-use-package vertico vterm
            vundo which-key z3-mode zig-mode))
 '(warning-minimum-level :emergency))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-delete ((t (:inherit nil :foreground "red3"))))
 '(flycheck-error ((t (:underline "Red1"))))
 '(flycheck-info ((t (:underline "ForestGreen"))))
 '(flycheck-warning ((t (:underline "DarkOrange"))))
 '(fringe ((t (:inherit default))))
 '(indent-guide-face ((t (:foreground "dark gray" :slant normal)))))

(provide 'custom)

;;; custom.el ends here
