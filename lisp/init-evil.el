;;; init-evil.el --- support for evil-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;install evil package

(require-package 'evil)
;; enable evil-mode
(evil-mode 1)
;;install evil-numbers
(require-package 'evil-numbers)

(define-key evil-normal-state-map (kbd "C-c =") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)


(provide 'init-evil)

;;; init-evil.el ends here
