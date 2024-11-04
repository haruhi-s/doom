;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb
(setq lsp-idle-delay 0.500)
(setq-default clang-format-executable "~/.local/bin/clang-format")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "haruhi suzumiya"
      user-mail-address "haruhi.s@proton.me")

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

(setq doom-font (font-spec :family "Cascadia Code" :size 26))
;; doom-variable-pitch-font (font-spec :family "Cascadia Code SemiBold" :size 30))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-horizon)

(setq doom-theme 'doom-gruvbox)
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
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-a") 'beginning-of-visual-line)
(global-set-key (kbd "C-e") 'end-of-visual-line)
(global-visual-line-mode 't)

(add-hook 'python-mode-hook (lambda () (setq forward-sexp-function nil)))

;; telega
(global-set-key (kbd "C-c C-t C-t") 'telega)
(global-set-key (kbd "C-c C-t t") 'telega)
(setq telega-avatar-workaround-gaps-for '(return t))
(setq telega-accounts '(("tomato" telega-database-dir "/home/a/.telega")
                        ("potata" telega-database-dir "/home/a/.potata")
                        ("tomatoin" telega-database-dir "/home/a/.tomatoin")))
(add-hook 'telega-load-hook
          (lambda ()
            (define-key global-map (kbd "C-c t") telega-prefix-map)))

(use-package swiper
  :config
  (global-set-key (kbd "M-t") 'swiper-thing-at-point)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-r") 'swiper-isearch-backward)
  )

(bind-key* "M-m b s" 'scratch-buffer)

(winum-mode)
(bind-key* "M-m w d" 'delete-window)
(bind-key* "M-m w /" 'split-window-horizontally)
(bind-key* "M-m w -" 'split-window-vertically)
(bind-key* "M-1" 'winum-select-window-1)
(bind-key* "M-2" 'winum-select-window-2)
(bind-key* "M-3" 'winum-select-window-3)
(bind-key* "M-4" 'winum-select-window-4)
(bind-key* "M-5" 'winum-select-window-5)

(setq-default ivy-use-virtual-buffers t)
(bind-key* "C-x C-b" 'ivy-switch-buffer)
(bind-key* "M-m b b" 'ivy-switch-buffer)
(bind-key* "M-m M-b" 'ibuffer)
(bind-key* "M-m b d" 'kill-this-buffer)
(bind-key* "M-m b p" 'previous-buffer)
(bind-key* "M-m b n" 'next-buffer)
(bind-key* "M-m M-p" 'previous-buffer)
(bind-key* "M-m M-n" 'next-buffer)
(bind-key* "M-m M-d" 'kill-this-buffer)
(bind-key* "M-m b w" 'read-only-mode)

(use-package paredit
  :config
    (define-key paredit-mode-map (kbd "C-j") nil)
    (define-key paredit-mode-map (kbd "C-m") nil)
    (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete))

(defun u-lisp-config ()
  (smartparens-mode -1)
  (flycheck-mode -1)
  (paredit-mode t)
  (paren-face-mode t)
  (rainbow-delimiters-mode -1))

(defun u-minibuffer-setup ()
  (when (memq this-command '(eval-expression))
    (u-lisp-config)))

(add-hook 'minibuffer-setup-hook 'u-minibuffer-setup)
(add-hook 'lisp-mode-hook 'u-lisp-config)
(add-hook 'emacs-lisp-mode-hook 'u-lisp-config)
(add-hook 'inferior-scheme-mode-hook 'u-lisp-config)

(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-print-last-sexp)

(define-key prog-mode-map (kbd "M-R") 'replace-string)
(define-key prog-mode-map (kbd "C-M-r") 'replace-regexp)
(define-key prog-mode-map (kbd "M-n") 'forward-paragraph)
(define-key prog-mode-map (kbd "M-p") 'backward-paragraph)

(add-hook 'haskell-interactive-mode-hook
          (lambda ()
            (local-set-key (kbd "C-a") 'haskell-interactive-mode-beginning)))

(bind-key* "C-M-p" 'backward-paragraph)
(bind-key* "C-M-n" 'forward-paragraph)

(bind-key* "M-z" 'hs-hide-level)
(bind-key* "M-j" 'hs-show-block)
(bind-key* "C-M-j" 'hs-show-block)
(bind-key* "M-k" 'hs-toggle-hiding)

(bind-key* "M-m h u" 'unhighlight-regexp)
(bind-key* "M-m h ." 'highlight-symbol-at-point)

(defun save-and-format-buffer ()
  (interactive)
  (clang-format-buffer)
  (save-buffer))
(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "C-x C-s") 'save-and-format-buffer)))

;; (after! flycheck
;;   (setq flycheck-global-modes '(not c-mode c++-mode)))

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
  (setq-default pyim-enable-shortcode nil)
  (require 'popup)
  (setq pyim-page-tooltip 'popup)
  )

(use-package vterm
  :bind
  (:map vterm-mode-map
   ("C-c C-j" . vterm-copy-mode)
   ("C-q" . vterm-send-next-key)
   ("C-k" . vterm-send-Ck)
   :map vterm-copy-mode-map
   ("C-c C-k" . (lambda () (interactive) (vterm-copy-mode -1))))
  :config
  (setq vterm-max-scrollback 1000000)
  (defun vterm-send-Ck ()
    "Send `C-k' to libvterm."
    (interactive)
    (kill-ring-save (point) (vterm-end-of-line))
    (vterm-send-key "k" nil nil t)))

(use-package multi-vterm
  :config
  (bind-key* (kbd "M-t M-t") 'multi-vterm) ;
  (bind-key* (kbd "M-t M-p") 'multi-vterm-prev)
  (bind-key* (kbd "M-t M-n") 'multi-vterm-next)
  (define-key vterm-mode-map (kbd "C-c C-j") 'vterm-copy-mode)
  (define-key vterm-copy-mode-map (kbd "C-c C-k") 'vterm-copy-mode)
  )

(use-package pyim-basedict
  :config
  (pyim-basedict-enable))

;; company
(use-package company
  :config
  (setf (cdr (assoc 'prog-mode +company-backend-alist)) '(company-capf company-files)))

;; auto-highlighting-mode
(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode 1)
  (setq ahs-idle-interval 0))

;; lisp
(use-package slime
  :config
  ;; (setq-default company-backends (cons 'company-slime (remove 'company-slime company-backends)))
  (setq-default inferior-lisp-program "sbcl")
  (slime-setup '(slime-company slime-fancy slime-quicklisp slime-asdf slime-media slime-parse slime-mrepl))
  (add-hook 'slime-mode-hook 'u-lisp-config)
  (add-hook 'slime-repl-mode-hook 'u-lisp-config)

  (define-key slime-mode-map (kbd "C-M-a") 'slime-beginning-of-defun)
  (define-key slime-mode-map (kbd "C-M-e") 'slime-end-of-defun)
  (define-key slime-mode-map (kbd "M-p") 'backward-paragraph)
  (define-key slime-mode-map (kbd "M-n") 'forward-paragraph)
  (define-key slime-mode-map (kbd "M-r") nil)
  (define-key slime-repl-mode-map (kbd "M-r") nil)
  )

(use-package org-download
  :ensure t
  :after org
  :config
  (setq-default
   org-download-image-dir "assets"
   ;; Basename setting seems to be simply ignored.
   org-download-screenshot-basename ".org.png"
   org-download-timestamp "org_%Y%m%d-%H%M%S_"
   org-download-heading-lvl nil)
  (defun paste-screenshot-to-telega ()
      (interactive)
    (shell-command-to-string
     (format org-download-screenshot-method
             org-download-screenshot-file))
    (let ((file  "/tmp/screenshot.png")
          (as-file-p current-prefix-arg))
      (telega-buffer-file-send
       file (telega-completing-read-chat
             (format "Send %s(%s) to chat: "
                     (cond ((listp file)
                            (format "%d FILES" (length file)))
                           (t "FILE"))
                     (if as-file-p
                         "as file"
                       "autodetect"))))))
  (global-set-key (kbd "C-M-y") 'paste-screenshot-to-telega)
  :custom
  (org-download-screenshot-method
   (cond
    ((eq system-type 'gnu/linux)
     "xclip -selection clipboard -t image/png -o > '%s'"))))

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

(use-package image-mode
  :config
  (define-key image-mode-map (kbd "=") 'image-increase-size)
  (define-key image-mode-map (kbd "-") 'image-decrease-size))
