;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb
(setq lsp-idle-delay 0.500)
(setq-default clang-format-executable "~/.local/bin/clang-format")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;

(setq doom-font (font-spec :family "Cascadia Code" :size 15))
;;      doom-variable-pitch-font (font-spec :family "Cascadia Code SemiBold" :size 30))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-horizon)

(setq doom-theme 'doom-vibrant)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq-default tab-always-indent t)
(setq display-line-numbers-type t)
(setq-default comment-line-break-function nil)
(setq-default indicate-empty-lines t)
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(set-fringe-bitmap-face 'tilde 'font-lock-function-name-face)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq kill-whole-line t)
(add-hook 'python-mode-hook (lambda () (setq forward-sexp-function nil)))

(global-set-key (kbd "C-c C-t C-t") 'telega)
(global-set-key (kbd "C-c C-t t") 'telega)

(global-set-key (kbd "M-t") 'swiper-thing-at-point)
(global-set-key (kbd "M-i") 'lsp-find-definition)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-a") 'beginning-of-visual-line)
(global-set-key (kbd "C-e") 'end-of-visual-line)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
(bind-key* "M-m b s" 'doom/open-scratch-buffer)

(global-visual-line-mode 't)
(winum-mode)
(bind-key* "M-m w d" 'delete-window)
(bind-key* "M-m w /" 'split-window-horizontally)
(bind-key* "M-m w -" 'split-window-vertically)
(bind-key* "M-1" 'winum-select-window-1)
(bind-key* "M-2" 'winum-select-window-2)
(bind-key* "M-3" 'winum-select-window-3)
(bind-key* "M-4" 'winum-select-window-4)
(bind-key* "M-5" 'winum-select-window-5)

;; (bind-key* "M-m b b" 'ido-switch-buffer)
;; (bind-key* "M-m b b" 'counsel-buffer-or-recentf)
(setq-default ivy-use-virtual-buffers t)
(bind-key* "M-m b b" 'ivy-switch-buffer)
(bind-key* "M-m b d" 'kill-this-buffer)
(bind-key* "M-m b p" 'previous-buffer)
(bind-key* "M-m b n" 'next-buffer)
(bind-key* "M-m b w" 'read-only-mode)

(add-hook 'prog-mode-hook (lambda ()
                            (local-set-key (kbd "M-p") 'backward-paragraph)
                            (local-set-key (kbd "M-n") 'forward-paragraph)
                            (local-set-key (kbd "M-r") 'replace-string)
                            ))

(bind-key* "C-M-p" 'backward-paragraph)
(bind-key* "C-M-n" 'forward-paragraph)

(bind-key* "M-z" 'hs-hide-level)
(bind-key* "M-j" 'hs-show-block)
(bind-key* "C-M-j" 'hs-show-block)
(bind-key* "M-k" 'hs-toggle-hiding)

(bind-key* "M-m h u" 'unhighlight-regexp)
(bind-key* "M-m h ." 'highlight-symbol-at-point)

(global-auto-highlight-symbol-mode)
(ahs-set-idle-interval 0.001)

(defun save-and-format-buffer ()
  (interactive)
  (clang-format-buffer)
  (save-buffer)
  )
(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "C-x C-s") 'save-and-format-buffer)))

(defun copy-all-or-region ()
  (interactive)
  (if (use-region-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (message "Copied selection.")
        (deactivate-mark))
    (progn
      (kill-new (buffer-string))
      (message "Copied buffer."))))
(global-set-key (kbd "M-w") 'copy-all-or-region)

(after! flycheck
  (setq flycheck-global-modes '(not c-mode c++-mode)))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; subwords for CamelCase
(use-package subword
  :config
  (global-subword-mode 't))

;; IM
(use-package pyim
  :config
  (setq pyim-page-length 5)
  (pyim-default-scheme 'microsoft-shuangpin)
  (global-set-key (kbd "M-i") 'pyim-deactivate)
  (global-set-key (kbd "M-o") 'pyim-activate)
  (define-key pyim-mode-map "." #'pyim-next-page)
  (define-key pyim-mode-map "," #'pyim-previous-page)
  (setq-default pyim-punctuation-translate-p '(no))
  (setq-default pyim-pinyin-fuzzy-alist '())
  (require 'popup)
  (setq pyim-page-tooltip 'popup)
  )

(use-package pyim-basedict
  :config
  (pyim-basedict-enable))

;; telega
(add-hook 'telega-load-hook
          (lambda ()
            (define-key global-map (kbd "C-c t") telega-prefix-map)))
(setq telega-avatar-workaround-gaps-for '(return t))

;; lisp

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-company slime-fancy slime-quicklisp slime-asdf slime-media slime-parse slime-mrepl))
  )

(use-package composite
  :defer t
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
    . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; support ligatures, some toned down to prevent hang
  (when (version<= "27.0" emacs-version)
    (let ((alist
           '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36 . ".\\(?:\\(>\\)>?\\)")
             (37 . ".\\(?:\\(%\\)%?\\)")
             (38 . ".\\(?:\\(&\\)&?\\)")
             (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43 . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45 . ".\\(?:\\(-[->]\\|[-<>|~]\\)[-<>|~]?\\)")
             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
             (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
             (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59 . ".\\(?:\\(;\\);?\\)")
             (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61 . ".\\(?:\\(!=\\|/=\\|:=\\|=[=>]\\|[=>]\\)[=<>]?\\)")
             (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91 . ".\\(?:\\(|\\)[]|]?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94 . ".\\(?:\\(=\\)=?\\)")
             (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\)[|}]?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table))
  )
