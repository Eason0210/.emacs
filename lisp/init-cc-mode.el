;;; init-cc-mode.el --- support for cc-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'counsel-gtags)
(require-package 'counsel-etags)

;;; Code for gtages

(defun gtags-ext-produce-tags-if-needed (dir)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
      (let ((default-directory dir))
        (shell-command "gtags")
        (message "tagfile created by GNU Global"))
    ;;  tagfile already exists; update it
    (shell-command "global -u")
    (message "tagfile updated by GNU Global")))

;; @see http://emacs-fu.blogspot.com.au/2008/01/navigating-through-source-code-using.html
(defun gtags-ext-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (gtags-ext-produce-tags-if-needed (read-directory-name
                                     "gtags: top of source tree:" default-directory)))

(defun gtags-ext-add-gtagslibpath (libdir &optional del)
  "Add external library directory to environment variable GTAGSLIBPATH.
Gtags will scan that directory if needed.
C-u M-x add-gtagslibpath will remove the directory from GTAGSLIBPATH."
  (interactive "DDirectory containing GTAGS:\nP")
  (let (sl)
    (if (not (file-exists-p (concat (file-name-as-directory libdir) "GTAGS")))
        ;; create tags
        (let ((default-directory libdir))
          (shell-command "gtags")
          (message "tagfile created by GNU Global")))

    (setq libdir (directory-file-name libdir)) ;remove final slash
    (setq sl (split-string (if (getenv "GTAGSLIBPATH") (getenv "GTAGSLIBPATH") "")  ":" t))
    (if del (setq sl (delete libdir sl)) (add-to-list 'sl libdir t))
    (setenv "GTAGSLIBPATH" (mapconcat 'identity sl ":"))
    ))

(defun gtags-ext-print-gtagslibpath ()
  "print the GTAGSLIBPATH (for debug purpose)"
  (interactive)
  (message "GTAGSLIBPATH=%s" (getenv "GTAGSLIBPATH")))

;;; code for ctags

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)

(when *is-a-mac*
  ;; Mac's default ctags does not support -e option
  ;; If you install Emacs by homebrew, another version of etags is already installed which does not need -e too
  ;; the best option is to install latest ctags from sf.net
  (setq ctags-command "/usr/local/bin/ctags -e -R "))


;;; cc-mode

(defun c-wx-lineup-topmost-intro-cont (langelem)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "EVT_" (line-end-position) t)
        'c-basic-offset
      (c-lineup-topmost-intro-cont langelem))))

;; avoid default "gnu" style, use more popular one
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

(defun my-common-cc-mode-setup ()
  "setup shared by all languages (java/groovy/c++ ...)"
  (setq c-basic-offset 4)
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)

  ;; syntax-highlight aggressively
  ;; (setq font-lock-support-mode 'lazy-lock-mode)
  (setq lazy-lock-defer-contextually t)
  (setq lazy-lock-defer-time 0)

                                        ;make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)

  ;; indent
  ;; google "C/C++/Java code indentation in Emacs" for more advanced skills
  ;; C code:
  ;;   if(1) // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
  ;;   void fn() // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0))

(defun my-c-mode-setup ()
  "C/C++ only setup."
  ;; @see http://stackoverflow.com/questions/3509919/ \
  ;; emacs-c-opening-corresponding-header-file
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)

  (setq cc-search-directories '("." "/usr/include" "/usr/local/include/*" "../*/include" "$WXWIN/include"))

  ;; {{ @see https://github.com/redguardtoo/cpputils-cmake
  ;; In theory, you can write your own Makefile for `flyamke-mode' without cmake.
  ;; Nobody actually does it in real world.
  ;; So make sure cmake is used before uncomment below code.

  ;; (when buffer-file-name
  ;;   (flymake-mode 1)
  ;;   (when (and (executable-find "cmake")
  ;;              (not (string-match-p "^\\(/usr/local/include\\|/usr/src/linux/include\\)/.*"
  ;;                                   buffer-file-name)))
  ;;     (cppcm-reload-all)))

  ;; }}

  ;; wxWidgets setup
  (c-set-offset 'topmost-intro-cont 'c-wx-lineup-topmost-intro-cont)

  ;; debugging Emacs c code
  (add-to-list 'imenu-generic-expression '(nil "^DEFUN *(\"\\([a-zA-Z0-9-]+\\)" 1))

  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior (quote (alignleft))))

;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(defun c-mode-common-hook-setup ()
  (unless (is-buffer-file-temp)
    (my-common-cc-mode-setup)
    (unless (or (derived-mode-p 'java-mode) (derived-mode-p 'groovy-mode))
      (my-c-mode-setup))

    ;; gtags (GNU global) stuff
    (when (and (executable-find "global")
               ;; `man global' to figure out why
               (not (string-match-p "GTAGS not found"
                                    (shell-command-to-string "global -p"))))
      ;; emacs 24.4+ will set up eldoc automatically.
      ;; so below code is NOT needed.
      (eldoc-mode 1))))
(add-hook 'c-mode-common-hook 'c-mode-common-hook-setup)


(provide 'init-cc-mode)

;;; init-cc-mode.el ends here
