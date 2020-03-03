;;; init-evil.el --- support for evil-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;install evil package

(require-package 'evil)
;; enable evil-mode
(evil-mode 1)

(require-package 'evil-numbers)
(require-package 'evil-escape)
(require-package 'evil-exchange)
;; (require-package 'evil-find-char-pinyin)
(require-package 'evil-iedit-state)
(require-package 'evil-mark-replace)
(require-package 'evil-matchit)
;; (require-package 'evil-nerd-commenter)
(require-package 'evil-surround)
(require-package 'evil-visualstar)
(require-package 'evil-lion)
(require-package 'evil-args)

;;install evil-numbers
;;install by site-lisp with a new version

(define-key evil-normal-state-map (kbd "C-c =") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;;evil-machit
(setq evilmi-shortcut "m")
(global-evil-matchit-mode 1)
(defvar my-use-m-for-matchit t
  "If t, use \"m\" key for `evil-matchit-mode'.
And \"%\" key is also retored to `evil-jump-item'.")

;;evil-surround
;; {{ @see https://github.com/timcharper/evil-surround for tutorial
(global-evil-surround-mode 1)
(defun evil-surround-prog-mode-hook-setup ()
  (push '(?$ . ("${" . "}")) evil-surround-pairs-alist)
  (push '(?/ . ("/" . "/")) evil-surround-pairs-alist))
(add-hook 'prog-mode-hook 'evil-surround-prog-mode-hook-setup)

(defun evil-surround-js-mode-hook-setup ()
  ;; ES6
  (push '(?> . ("(e) => " . "(e)")) evil-surround-pairs-alist))
(add-hook 'js-mode-hook 'evil-surround-js-mode-hook-setup)

(defun evil-surround-emacs-lisp-mode-hook-setup ()
  (push '(?\( . ("( " . ")")) evil-surround-pairs-alist)
  (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
(add-hook 'emacs-lisp-mode-hook 'evil-surround-emacs-lisp-mode-hook-setup)

(defun evil-surround-org-mode-hook-setup ()
  (push '(93 . ("[[" . "]]")) evil-surround-pairs-alist) ; ]
  (push '(?= . ("=" . "=")) evil-surround-pairs-alist))
(add-hook 'org-mode-hook 'evil-surround-org-mode-hook-setup)
;; }}

;;evil-visualstar
;; {{ For example, press `viW*`
(require-package 'evil-visualstar)
(setq evil-visualstar/persistent t)
(global-evil-visualstar-mode t)
;; }}

;; TODO
;; ffip-diff-mode (read only) evil setup
;; (defun ffip-diff-mode-hook-setup ()
;;   (evil-local-set-key 'normal "q" (lambda () (interactive) (quit-window t)))
;;   (evil-local-set-key 'normal (kbd "RET") 'ffip-diff-find-file)
;;   ;; "C-c C-a" is binding to `diff-apply-hunk' in `diff-mode'
;;   (evil-local-set-key 'normal "a" 'ffip-diff-apply-hunk)
;;   (evil-local-set-key 'normal "o" 'ffip-diff-find-file))
;; (add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-setup)

;; Text object
;; {{ define my own text objects, works on evil v1.0.9 using older method
;; @see http://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp
(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let* ((inner-name (make-symbol "inner-name"))
         (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;; between dollar signs:
(define-and-bind-text-object "$" "\\$" "\\$")
;; between equal signs
(define-and-bind-text-object "=" "=" "=")
;; between pipe characters:
(define-and-bind-text-object "|" "|" "|")
;; regular expression
(define-and-bind-text-object "/" "/" "/")
;; trimmed line
(define-and-bind-text-object "l" "^ *" " *$")
;; angular template
(define-and-bind-text-object "r" "\{\{" "\}\}")
;; }}



;; {{ nearby file path as text object,
;;      - "vif" to select base name
;;      - "vaf" to select full path
;;
;;  example:
;;    "/hello/world"
;;    "/test/back.exe"
;;    "C:hello\\hello\\world\\test.exe"
;;    "D:blah\\hello\\world\\base.exe"
(defun evil-filepath-is-separator-char (ch)
  "Check ascii table that CH is slash characters.
If the character before and after CH is space or tab, CH is NOT slash"
  (let* (rlt prefix-ch postfix-ch)
    (when (and (> (point) (point-min)) (< (point) (point-max)))
      (save-excursion
        (backward-char)
        (setq prefix-ch (following-char)))
      (save-excursion
        (forward-char)
        (setq postfix-ch (following-char))))
    (if (and (not (or (= prefix-ch 32) (= postfix-ch 32)))
             (or (= ch 47) (= ch 92)) )
        (setq rlt t))
    rlt))

(defun evil-filepath-not-path-char (ch)
  "Check ascii table for charctater."
  (or (and (<= 0 ch) (<= ch 32))
      (memq ch
            '(34 ; double quotes
              ?'
              40 ; (
              41 ; )
              ?<
              ?>
              91 ; [
              93 ; ]
              ?`
              ?{
              ?}
              127))))

(defun evil-filepath-calculate-path (b e)
  (let* (rlt f)
    (when (and b e)
      (setq b (+ 1 b))
      (when (save-excursion
              (goto-char e)
              (setq f (evil-filepath-search-forward-char 'evil-filepath-is-separator-char t))
              (and f (>= f b)))
        (setq rlt (list b (+ 1 f) (- e 1)))))
    rlt))

(defun evil-filepath-get-path-already-inside ()
  (let* (b e)
    (save-excursion
      (setq b (evil-filepath-search-forward-char 'evil-filepath-not-path-char t)))
    (save-excursion
      (when (setq e (evil-filepath-search-forward-char 'evil-filepath-not-path-char))
        (goto-char (- e 1))
        ;; example: hello/world,
        (if (memq (following-char) '(?, ?.))
            (setq e (- e 1)))))
    (evil-filepath-calculate-path b e)))

(defun evil-filepath-search-forward-char (fn &optional backward)
  (let* (found
         rlt
         (limit (if backward (point-min) (point-max)))
         out-of-loop)
    (save-excursion
      (while (not out-of-loop)
        ;; for the char, exit
        (if (setq found (apply fn (list (following-char))))
            (setq out-of-loop t)
          ;; reach the limit, exit
          (if (= (point) limit)
              (setq out-of-loop t)
            ;; keep moving
            (if backward (backward-char) (forward-char)))))
      (if found (setq rlt (point))))
    rlt))

(defun evil-filepath-extract-region ()
  "Find the closest file path"
  (let* (rlt b f1 f2)
    (if (and (not (evil-filepath-not-path-char (following-char)))
             (setq rlt (evil-filepath-get-path-already-inside)))
        ;; maybe (point) is in the middle of the path
        t
      ;; need search forward AND backward to find the right path
      (save-excursion
        ;; path in backward direction
        (when (setq b (evil-filepath-search-forward-char 'evil-filepath-is-separator-char t))
          (goto-char b)
          (setq f1 (evil-filepath-get-path-already-inside))))
      (save-excursion
        ;; path in forward direction
        (when (setq b (evil-filepath-search-forward-char 'evil-filepath-is-separator-char))
          (goto-char b)
          (setq f2 (evil-filepath-get-path-already-inside))))
      ;; pick one path as the final result
      (cond
       ((and f1 f2)
        (if (> (- (point) (nth 2 f1)) (- (nth 0 f2) (point)))
            (setq rlt f2)
          (setq rlt f1)))
       (f1
        (setq rlt f1))
       (f2
        (setq rlt f2))))

    rlt))

(evil-define-text-object evil-filepath-inner-text-object (&optional count begin end type)
  "File name of nearby path"
  (let* ((selected-region (evil-filepath-extract-region)))
    (if selected-region
        (evil-range (nth 1 selected-region) (nth 2 selected-region) :expanded t))))

(evil-define-text-object evil-filepath-outer-text-object (&optional NUM begin end type)
  "Nearby path."
  (let* ((selected-region (evil-filepath-extract-region)))
    (if selected-region
        (evil-range (car selected-region) (+ 1 (nth 2 selected-region)) type :expanded t))))

(define-key evil-inner-text-objects-map "f" 'evil-filepath-inner-text-object)
(define-key evil-outer-text-objects-map "f" 'evil-filepath-outer-text-object)
;; }}

;; evil-escape TODO , need to check
;; {{ https://github.com/syl20bnr/evil-escape
(require-package 'evil-escape)
(setq-default evil-escape-delay 0.3)
(setq evil-escape-excluded-major-modes '(dired-mode))
(setq-default evil-escape-key-sequence "kj")
;; disable evil-escape when input method is on
(evil-escape-mode 1)

;; }}


;; Key binding
(define-key evil-normal-state-map "Y" (kbd "y$"))

(define-key evil-normal-state-map "go" 'goto-char)
(define-key evil-normal-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)
(define-key evil-visual-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)
;; (define-key evil-insert-state-map (kbd "C-x C-n") 'evil-complete-next-line)
;; (define-key evil-insert-state-map (kbd "C-x C-p") 'evil-complete-previous-line)


(defun my-search-defun-from-pos (search pos)
  (evil-search search t t pos)
  ;; ignore this.f1 = this.fn.bind(this) code
  (when (and (memq major-mode '(js-mode js2-mode rjsx-mode))
             (string-match-p "^[ \t]*this\.[a-zA-Z0-9]+[ \t]*=[ \t]*this\.[a-zA-Z0-9]*\.bind(this);"
                             (my-line-str)))

    (forward-line 1)
    (evil-search search t t (point))))

;; "gd" or `evil-goto-definition' now use `imenu', `xref' first,
;; BEFORE searching string from `point-min'.
;; xref part is annoying because I already use `counsel-etags' to search tag.
(evil-define-motion my-evil-goto-definition ()
  "Go to definition or first occurrence of symbol under point in current buffer."
  :jump t
  :type exclusive
  (let* ((string (evil-find-symbol t))
         (search (format "\\_<%s\\_>" (regexp-quote string)))
         ientry ipos)
    ;; load imenu if available
    (my-ensure 'imenu)

    (if (null string)
        (user-error "No symbol under cursor")
      (setq isearch-forward t)
      ;; if imenu is available, try it
      (cond
       ((and (derived-mode-p 'js2-mode)
             (or (null (get-text-property (point) 'face))
                 (font-belongs-to (point) '(rjsx-tag))))
        (js2-jump-to-definition))
       ((fboundp 'imenu--make-index-alist)
        (condition-case nil
            (setq ientry (imenu--make-index-alist))
          (error nil))
        (setq ientry (assoc string ientry))
        (setq ipos (cdr ientry))
        (when (and (markerp ipos)
                   (eq (marker-buffer ipos) (current-buffer)))
          (setq ipos (marker-position ipos)))
        ;; imenu found a position, so go there and
        ;; highlight the occurrence
        (my-search-defun-from-pos search (if (numberp ipos) ipos (point-min))))
       ;; otherwise just go to first occurrence in buffer
       (t
        (my-search-defun-from-pos search (point-min)))))))
;; use "gt", someone might prefer original `evil-goto-definition'
(define-key evil-motion-state-map "gt" 'my-evil-goto-definition)


;; TODO
;; ;; I learn this trick from ReneFroger, need latest expand-region
;; ;; @see https://github.com/redguardtoo/evil-matchit/issues/38
;; (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
;; (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
;; (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
;; (define-key evil-insert-state-map (kbd "M-j") 'yas-expand)
;; (define-key evil-emacs-state-map (kbd "M-j") 'yas-expand)
;; (global-set-key (kbd "C-r") 'undo-tree-redo)


;; Global marker
;; {{
(defvar evil-global-markers-history nil)
(defadvice evil-set-marker (before evil-set-marker-before-hack activate)
  (let* ((args (ad-get-args 0))
         (c (nth 0 args))
         (pos (or (nth 1 args) (point))))
    ;; only rememeber global markers
    (when (and (>= c ?A) (<= c ?Z) buffer-file-name)
      (setq evil-global-markers-history
            (delq nil
                  (mapcar `(lambda (e)
                             (unless (string-match (format "^%s@" (char-to-string ,c)) e)
                               e))
                          evil-global-markers-history)))
      (setq evil-global-markers-history
            (add-to-list 'evil-global-markers-history
                         (format "%s@%s:%d:%s"
                                 (char-to-string c)
                                 (file-truename buffer-file-name)
                                 (line-number-at-pos pos)
                                 (string-trim (my-line-str))))))))

(defadvice evil-goto-mark-line (around evil-goto-mark-line-hack activate)
  (let* ((args (ad-get-args 0))
         (c (nth 0 args))
         (orig-pos (point)))

    (condition-case nil
        ad-do-it
      (error (progn
               (when (and (eq orig-pos (point)) evil-global-markers-history)
                 (let* ((markers evil-global-markers-history)
                        (i 0)
                        m
                        file
                        found)
                   (while (and (not found) (< i (length markers)))
                     (setq m (nth i markers))
                     (when (string-match (format "\\`%s@\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'"
                                                 (char-to-string c))
                                         m)
                       (setq file (match-string-no-properties 1 m))
                       (setq found (match-string-no-properties 2 m)))
                     (setq i (1+ i)))
                   (when file
                     (find-file file)
                     (counsel-etags-forward-line found)))))))))

(defun counsel-evil-goto-global-marker ()
  "Goto global evil marker."
  (interactive)
  (my-ensure 'counsel-etags)
  (ivy-read "Goto global evil marker"
            evil-global-markers-history
            :action (lambda (m)
                      (when (string-match "\\`[A-Z]@\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" m)
                        (let* ((file (match-string-no-properties 1 m))
                               (linenum (match-string-no-properties 2 m)))
                          ;; item's format is like '~/proj1/ab.el:39: (defun hello() )'
                          (counsel-etags-push-marker-stack (point-marker))
                          ;; open file, go to certain line
                          (find-file file)
                          (counsel-etags-forward-line linenum))
                        ;; flash, Emacs v25 only API
                        (xref-pulse-momentarily)))))
;; }}



(require-package 'general)
(general-evil-setup t)

;; {{ use `,` as leader key
(general-create-definer my-comma-leader-def
                        :prefix ","
                        :states '(normal visual))

(my-comma-leader-def
  "bf" 'beginning-of-defun
  "bu" 'backward-up-list
  "bb" (lambda () (interactive) (switch-to-buffer nil)) ; to previous buffer
  "ef" 'end-of-defun
  "m" 'evil-set-marker
  "em" 'erase-message-buffer
  "eb" 'eval-buffer
  ;; "sd" 'sudo-edit
  "sc" 'scratch
  "ee" 'eval-expression
  "aa" 'copy-to-x-clipboard ; used frequently
  ;; "aw" 'ace-swap-window
  "sw" 'switch-window-then-swap-buffer
  "af" 'ace-maximize-window
  ;; "ac" 'aya-create
  "pp" 'paste-from-x-clipboard ; used frequently
  "bs" '(lambda () (interactive) (goto-edge-by-comparing-font-face -1))
  "es" 'goto-edge-by-comparing-font-face
  ;; "vj" 'my-validate-json-or-js-expression
  "kc" 'kill-ring-to-clipboard
  "fn" 'cp-filename-of-current-buffer
  "fp" 'cp-fullpath-of-current-buffer
  "dj" 'dired-jump ;; open the dired from current file
  "xd" 'dired
  ;; "xo" 'ace-window
  "xo" 'switch-window
  "ff" 'toggle-full-winow ;; I use WIN+F in i3
  "ip" 'find-file-in-project
  "tt" 'find-file-in-current-directory
  "jj" 'find-file-in-project-at-point
  "kk" 'find-file-in-project-by-selected
  "kn" 'find-file-with-similar-name ; ffip v5.3.1
  "fd" 'find-directory-in-project-by-selected
  "trm" 'get-term
  "tff" 'toggle-frame-fullscreen
  "tfm" 'toggle-frame-maximized
  "ti" 'fastdef-insert
  "th" 'fastdef-insert-from-history
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'my-evilnc-comment-or-uncomment-paragraphs
  "ct" 'evilnc-comment-or-uncomment-html-tag ; evil-nerd-commenter v3.3.0 required
  "ic" 'my-imenu-comments
  ;; {{ window move
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wk" 'evil-window-up
  "wj" 'evil-window-down
  ;; }}
  "rv" 'evilmr-replace-in-defun
  "rb" 'evilmr-replace-in-buffer
  "ts" 'evilmr-tag-selected-region ;; recommended
  "cby" 'cb-switch-between-controller-and-view
  "cbu" 'cb-get-url-from-controller
  "rt" 'counsel-etags-recent-tag
  "ft" 'counsel-etags-find-tag
  "yy" 'counsel-browse-kill-ring
  "cf" 'counsel-grep ; grep current buffer
  "gf" 'counsel-git ; find file
  "gg" 'my-counsel-git-grep ; quickest grep should be easy to press
  "gd" 'ffip-show-diff-by-description ;find-file-in-project 5.3.0+
  "gl" 'my-git-log-trace-definition ; find history of a function or range
  "sh" 'my-select-from-search-text-history
  "rjs" 'run-js
  "jsr" 'js-send-region
  "jsb" 'js-clear-send-buffer
  "kb" 'kill-buffer-and-window ;; "k" is preserved to replace "C-g"
  "ls" 'highlight-symbol
  "lq" 'highlight-symbol-query-replace
  "ln" 'highlight-symbol-nav-mode ; use M-n/M-p to navigation between symbols
  "ii" 'my-imenu-or-list-tag-in-current-file
  "ij" 'rimenu-jump
  "." 'evil-ex
  ;; @see https://github.com/pidu/git-timemachine
  ;; p: previous; n: next; w:hash; W:complete hash; g:nth version; q:quit
  "tg" 'dumb-jump-go
  "tb" 'dumb-jump-back
  "tm" 'my-git-timemachine
  ;; toggle overview,  @see http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
  "ov" 'my-overview-of-current-buffer
  "oo" 'compile
  "c$" 'org-archive-subtree ; `C-c $'
  ;; org-do-demote/org-do-premote support selected region
  "c<" 'org-do-promote ; `C-c C-<'
  "c>" 'org-do-demote ; `C-c C->'
  "cam" 'org-tags-view ; `C-c a m': search items in org-file-apps by tag
  "cxi" 'org-clock-in ; `C-c C-x C-i'
  "cxo" 'org-clock-out ; `C-c C-x C-o'
  "cxr" 'org-clock-report ; `C-c C-x C-r'
  "qq" 'my-multi-purpose-grep
  "dd" 'counsel-etags-grep-current-directory
  "rr" 'undo-tree-redo
  ;; "rr" 'my-counsel-recentf
  "rh" 'counsel-yank-bash-history ; bash history command => yank-ring
  "rd" 'counsel-recent-directory
  "da" 'diff-region-tag-selected-as-a
  "db" 'diff-region-compare-with-b
  "di" 'evilmi-delete-items
  "si" 'evilmi-select-items
  "jb" 'js-beautify
  "jp" 'my-print-json-path
  "xe" 'eval-last-sexp
  "x0" 'delete-window
  "wo" 'delete-other-windows
  "aw" 'sanityinc/split-window
  "x1" 'sanityinc/toggle-delete-other-windows
  "x2" (split-window-func-with-other-buffer 'split-window-vertically)
  "x3" (split-window-func-with-other-buffer 'split-window-horizontally)
  "x\\" 'split-window-horizontally-instead
  "x-" 'split-window-vertically-instead
  "xu" 'undo-tree-visualize
  "ru" 'undo-tree-save-state-to-register ; C-x r u
  "rU" 'undo-tree-restore-state-from-register ; C-x r U
  "xt" 'toggle-two-split-window
  "uu" 'winner-undo
  "ur" 'winner-redo
  "to" 'toggle-web-js-offset
  "fs" 'ffip-save-ivy-last
  "fr" 'ffip-ivy-resume
  "fc" 'cp-ffip-ivy-last
  "ss" 'my-counsel-grep-or-swiper
  "hd" 'describe-function
  "hf" 'find-function
  "hk" 'describe-key
  "hv" 'describe-variable
  "gt" 'counsel-gtags-dwim ; jump from reference to definition or vice versa
  "gr" 'counsel-gtags-find-symbol
  "gu" 'counsel-gtags-update-tags
  "fb" 'flyspell-buffer
  "fe" 'flyspell-goto-next-error
  "fa" 'flyspell-auto-correct-word
  "lb" 'langtool-check-buffer
  "ll" 'langtool-goto-next-error
  ;; "pe" 'flymake-goto-prev-error
  ;; "ne" 'flymake-goto-next-error
  "pe" 'flycheck-error-list-previous-error
  "ne" 'flycheck-error-list-next-error
  "og" 'org-agenda
  "otl" 'org-toggle-link-display
  "oa" '(lambda ()
          (interactive)
          (my-ensure 'org)
          (counsel-org-agenda-headlines))
  "om" 'toggle-org-or-message-mode
  "ut" 'undo-tree-visualize
  "ar" 'align-regexp
  "wrn" 'httpd-restart-now
  "wrd" 'httpd-restart-at-default-directory
  "bk" 'buf-move-up
  "bj" 'buf-move-down
  "bh" 'buf-move-left
  "bl" 'buf-move-right
  ;; "0" 'winum-select-window-0-or-10
  ;; "1" 'winum-select-window-1
  ;; "2" 'winum-select-window-2
  ;; "3" 'winum-select-window-3
  ;; "4" 'winum-select-window-4
  ;; "5" 'winum-select-window-5
  ;; "6" 'winum-select-window-6
  ;; "7" 'winum-select-window-7
  ;; "8" 'winum-select-window-8
  ;; "9" 'winum-select-window-9
  "xm" 'counsel-M-x
  "xx" 'er/expand-reion
  "xf" 'counsel-find-file
  ;; "xb" 'ivy-switch-buffer-by-pinyin
  "xb" 'switch-to-buffer
  "xh" 'mark-whole-buffer
  "xk" 'kill-buffer
  "xs" 'save-buffer
  "xc" 'save-buffers-kill-terminal
  "xz" 'my-switch-to-shell
  "vf" 'vc-rename-file-and-buffer
  "vc" 'vc-copy-file-and-rename-buffer
  "xv" 'vc-next-action ; 'C-x v v' in original
  "va" 'git-add-current-file
  "vk" 'git-checkout-current-file
  "vg" 'vc-annotate ; 'C-x v g' in original
  "vv" 'vc-msg-show
  "v=" 'git-gutter:popup-hunk
  "hh" 'cliphist-paste-item
  "yu" 'cliphist-select-item
  "ih" 'my-goto-git-gutter ; use ivy-mode
  "ir" 'ivy-resume
  "ww" 'narrow-or-widen-dwim
  "ycr" 'my-yas-reload-all
  "wf" 'popup-which-function)
;; }}


;; {{ Use `SPC` as leader key
;; all keywords arguments are still supported
(general-create-definer my-space-leader-def
                        :prefix "SPC"
                        :states '(normal visual))

(my-space-leader-def
  "ee" 'my-swap-sexps
  "nn" 'my-goto-next-hunk
  "pp" 'my-goto-previous-hunk
  "pc" 'my-dired-redo-from-commands-history
  "pw" 'pwd
  "mm" 'counsel-evil-goto-global-marker
  "mf" 'mark-defun
  "xc" 'save-buffers-kill-terminal ; not used frequently
  "cc" 'my-dired-redo-last-command
  "ss" 'wg-create-workgroup ; save windows layout
  "se" 'iedit-mode ;evil-iedit-state/ ; start iedit in emacs
  "sc" 'shell-command
  "ll" 'my-wg-switch-workgroup ; load windows layout
  "jj" 'scroll-other-window
  "kk" 'scroll-other-window-down
  "rt" 'random-healthy-color-theme
  ;; "yy" 'hydra-launcher/body
  "tt" 'my-toggle-indentation
  ;; "g" 'hydra-git/body
  "ps" 'profiler-start
  "pr" 'profiler-report
  "ud" 'my-gud-gdb
  "uk" 'gud-kill-yes
  "ur" 'gud-remove
  "ub" 'gud-break
  "uu" 'gud-run
  "up" 'gud-print
  "ue" 'gud-cls
  "un" 'gud-next
  "us" 'gud-step
  "ui" 'gud-stepi
  "uc" 'gud-cont
  "uf" 'gud-finish)

;; {{ Use `;` as leader key, for searching something
(general-create-definer my-semicolon-leader-def
  :prefix ";"
  :states '(normal visual))

(my-semicolon-leader-def
 ;; Search character(s) at the beginning of word
 ;; See https://github.com/abo-abo/avy/issues/70
 ;; You can change the avy font-face in ~/.custom.el:
 ;;  (eval-after-load 'avy
 ;;   '(progn
 ;;      (set-face-attribute 'avy-lead-face-0 nil :foreground "black")
 ;;      (set-face-attribute 'avy-lead-face-0 nil :background "#f86bf3")))
 ";" 'ace-pinyin-jump-char-2
 "w" 'avy-goto-word-or-subword-1
 "a" 'avy-goto-char-timer
 "db" 'sdcv-search-input ; details
 "dt" 'sdcv-search-input+ ; summary
 "dd" 'my-lookup-dict-org
 "mm" 'lookup-doc-in-man
 "gg" 'w3m-google-search
 "gd" 'w3m-search-financial-dictionary
 "ga" 'w3m-java-search
 "gh" 'w3mext-hacker-search ; code search in all engines with firefox
 "gq" 'w3m-stackoverflow-search)
;; }}


;; {{ remember what we searched
;; http://emacs.stackexchange.com/questions/24099/how-to-yank-text-to-search-command-after-in-evil-mode/
(defvar my-search-text-history nil "List of text I searched.")
(defun my-select-from-search-text-history ()
  (interactive)
  (ivy-read "Search text history:" my-search-text-history
            :action (lambda (item)
                      (copy-yank-str item)
                      (message "%s => clipboard & yank ring" item))))
(defun my-cc-isearch-string ()
  (interactive)
  (if (and isearch-string (> (length isearch-string) 0))
      ;; NOT pollute clipboard who has things to paste into Emacs
      (add-to-list 'my-search-text-history isearch-string)))

(defadvice evil-search-incrementally (after evil-search-incrementally-after-hack activate)
  (my-cc-isearch-string))

(defadvice evil-search-word (after evil-search-word-after-hack activate)
  (my-cc-isearch-string))

(defadvice evil-visualstar/begin-search (after evil-visualstar/begin-search-after-hack activate)
  (my-cc-isearch-string))
;; }}

;; TODO
;; ;; change mode-line color by evil state
;; (let* ((default-color (cons (face-background 'mode-line)
;;                             (face-foreground 'mode-line))))
;;   (add-hook 'post-command-hook
;;             (lambda ()
;;               (let* ((color (cond ((minibufferp) default-color)
;;                                   ((evil-insert-state-p) '("#83a598" . "#ffffff"))
;;                                   ((evil-visual-state-p)  '("#d79921" . "#ffffff"))
;;                                   ((evil-emacs-state-p)  '("#8ec07c" . "#ffffff"))
;;                                   ((buffer-modified-p)   '("#458588" . "#ffffff"))
;;                                   (t default-color))))
;;                 (set-face-background 'mode-line (car color))
;;                 (set-face-foreground 'mode-line (cdr color))))))


;; {{ evil-exchange
;; press gx twice to exchange, gX to cancel
;; change default key bindings (if you want) HERE
;; (setq evil-exchange-key (kbd "zx"))
(evil-exchange-install)
;; }}

;; {{ evil-lion
;; After pressing `glip=` or `gl2j=` (gl is the operator, ip or 2j is text object, = separator):
;; one = 1
;; three = 3
;; fifteen = 15
;;
;; will become:
;; one     = 1
;; three   = 3
;; fifteen = 15
;;
;; If the align separator is / you will be prompted for a regular expression instead of a plain character.
(evil-lion-mode)
;; }}

;; {{ @see https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org#replacing-text-with-iedit
;; same keybindgs as spacemacs:
;;  - "SPC s e" to start `iedit-mode'
;;  - "TAB" to toggle current occurrence
;;  - "n" next, "N" previous (obviously we use "p" for yank)
;;  - "gg" the first occurence, "G" the last occurence
;;  - Please note ";;" or `avy-goto-char-timer' is also useful
;; }}


;; {{ Port of vim-textobj-syntax.
;; It provides evil text objects for consecutive items with same syntax highlight.
(require-package 'evil-textobj-syntax)
;; }}

;; {{ evil-args
;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; bind evil-forward/backward-args
(define-key evil-normal-state-map "L" 'evil-forward-arg)
(define-key evil-normal-state-map "H" 'evil-backward-arg)
(define-key evil-motion-state-map "L" 'evil-forward-arg)
(define-key evil-motion-state-map "H" 'evil-backward-arg)

;; bind evil-jump-out-args
(define-key evil-normal-state-map "K" 'evil-jump-out-args)
;; }}

;; In insert mode, press "rr" in 0.2 second to trigger my-counsel-company
(general-imap "r"
  (general-key-dispatch 'self-insert-command
    :timeout 0.2
    "r" 'my-counsel-company))

(defun my-switch-to-shell ()
  "Switch to built in or 3rd party shell."
  (interactive)
  (cond
   ((display-graphic-p)
    (switch-to-builtin-shell))
   (t
    (suspend-frame))))


;; press ",xx" to expand region
;; then press "c" to contract, "x" to expand
(eval-after-load "evil"
  '(progn
     ;; evil re-assign "M-." to `evil-repeat-pop-next` which I don't use actually.
     ;; Restore "M-." to original binding command
     (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
     (setq expand-region-contract-fast-key "c")
     ;; @see https://bitbucket.org/lyro/evil/issue/360/possible-evil-search-symbol-forward
     ;; evil 1.0.8 search word instead of symbol
     (setq evil-symbol-word-search t)

     ;; don't add replaced text to `kill-ring'
     (setq evil-kill-on-visual-paste nil)

     ;; @see https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
     ;; uncomment below line to make "dw" has exact same behaviour in evil as as in vim
     ;; (defalias #'forward-evil-word #'forward-evil-symbol)

     ;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
     (defmacro adjust-major-mode-keymap-with-evil (m &optional r)
       `(eval-after-load (quote ,(if r r m))
          '(progn
             (evil-make-overriding-map ,(intern (concat m "-mode-map")) 'normal)
             ;; force update evil keymaps after git-timemachine-mode loaded
             (add-hook (quote ,(intern (concat m "-mode-hook"))) #'evil-normalize-keymaps))))

     (adjust-major-mode-keymap-with-evil "git-timemachine")

     ;; @see https://bitbucket.org/lyro/evil/issue/342/evil-default-cursor-setting-should-default
     ;; Cursor is always black because of evil.
     ;; Here is the workaround
     (setq evil-default-cursor t)))

(provide 'init-evil)

;;; init-evil.el ends here
