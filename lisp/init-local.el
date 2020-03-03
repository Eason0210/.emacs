;;; init-local.el --- Configure default locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst *is-a-win64* (eq system-type 'windows-nt))
;;Toggle input method
(defun evil-toggle-input-method ()
  "when toggle on input method, switch to evil-insert-state if possible.
    when toggle off input method, switch to evil-normal-state if current state is evil-insert-state"
  (interactive)
  (if (not current-input-method)
      (if (not (string= evil-state "insert"))
          (evil-insert-state))
    (if (string= evil-state "insert")
        (evil-normal-state)
      ))
  (toggle-input-method))

(global-set-key (kbd "C-\\") 'evil-toggle-input-method)

(setq comint-scroll-show-maximum-output nil)
(require-package 'gruvbox-theme)
(setq-default custom-enabled-themes '(gruvbox-dark-medium))
;; (setq inhibit-compacting-font-caches t)


;;Windows system refered setting
;;{{
(when *is-a-win64*

  ;;Python shell setting
  ;; (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i")
  (setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
  (setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
  (setq python-shell-completion-setup-code "from IPython.core.completerlib import module_completion")

  (setq python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n")
  (setq python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

  ;;Font setting
  (defun set-font (english chinese english-size chinese-size)
    (set-face-attribute 'default nil :font
                        (format   "%s:pixelsize=%d"  english english-size))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese :size chinese-size))))

  (set-font "DejaVu Sans Mono" "楷体" 20 22)

  )

;;}}
;; org-bullets mode
(require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; vimrc support
(require-package 'vimrc-mode)
(add-auto-mode 'vimrc-mode "\\.?vim\\(rc\\)?$")
;; iedit
(require-package 'iedit)
;; find file
(require-package 'find-file-in-project)

(setq imenu-max-item-length 256)

;;Highlight current line
(global-hl-line-mode 1)

(provide 'init-local)
;;; init-local.el ends here
