(add-to-list 'load-path "~/.emacs.d/elisp")

;;------- Enviroment ---------

(column-number-mode)
(setq fill-column 59)
(setq-default fill-column 72)

;; There's no place like home
(setq default-directory "~/")

;; ssh and local sudo/su
(require 'tramp)

;; splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)

;; utf-8 for good
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; encoding
(setq current-language-environment "UTF-8")

;; clipboard
(setq x-select-enable-clipboard t)

;; disable backup files
;; (setq make-backup-files nil)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; show trailing whitespace
(setq show-trailing-whitespace t)

;; Reloading the buffer instead of pissing me off with "what should I
;; do" questions
(defun ask-user-about-supersession-threat (filename)
  (revert-buffer t t)
  (message "This buffer was refreshed due to external changes"))

;; Adding marmalade as a repo to the package module
(require 'package)
(add-to-list
 'package-archives
 '("marmalade" .
   "http://marmalade-repo.org/packages/")
 '("melpa" .
   "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Mac specific stuff
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  ;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char)
  (menu-bar-mode 1))

;;-------- Modes --------

;; Css
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level 4)

;; sass and haml mode
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-hook 'markdown-mode-hook '(lambda() (flyspell-mode)))

;; Web Mode
(add-to-list 'load-path "~/.emacs.d/elisp/web-mode")
(require 'web-mode)

(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))

(add-hook 'web-mode-hook 'web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig?\\'" . web-mode))

;; Zencoding
(require 'zencoding-mode)
(add-hook 'web-mode-hook 'zencoding-mode)

(eval-after-load zencoding-mode
  (progn
    (define-key zencoding-mode-keymap (kbd "C-j") nil)))

;; Editorconfig
(load "editorconfig")

;; Slim-mode
(require 'slim-mode)

;;-------- Interface --------

;; no bars
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; setting up a color theme
(require 'monokai-theme)

;; setting up a color theme
;; (add-to-list 'load-path "~/.emacs.d/elisp/color-theme")
;; (require 'color-theme)
;; (load-file
;; "~/.emacs.d/elisp/color-theme/themes/color-theme-comum/color-theme-comum.el")
;; (color-theme-comum)

;; show line numbers
(require 'linum)
(global-linum-mode 1)

;; Always do syntax highlighting
(global-font-lock-mode 1)

;; Also highlight parens
(setq show-paren-delay 0 show-paren-style 'parenthesis)
(show-paren-mode 1)

;; highlight mark region
(transient-mark-mode 1)

;; respecting boundaries
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "#333333")

;; scroll smoothly
(setq scroll-conservatively 10000)

;;-------- Keybinds --------

;; change window
(global-set-key [C-tab] 'other-window)

;; comments
(global-set-key [(ctrl c) (c)] 'comment-region)
(global-set-key [(ctrl c) (d)] 'uncomment-region)

;; delete trailing whitespace
(global-set-key [(ctrl x) (w)] 'delete-trailing-whitespace)

;; buffer
(global-set-key [A-tab] 'next-buffer)
(global-set-key [S-A-iso-lefttab] 'previous-buffer)

;; Navegation
(global-set-key (kbd "s-g") 'goto-line)

;; Multiple Cursors
(global-set-key (kbd "C-c C-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;-------- Hooks ----------

;; Remove whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(put 'downcase-region 'disabled nil)

;; CSS color values colored by themselves
;; http://news.ycombinator.com/item?id=873541
(defvar hexcolor-keywords
  '(("#[abcdef[:digit:]]+"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :foreground (match-string-no-properties 0)))))))

(defun hexcolor-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolor-keywords))

(add-hook 'css-mode-hook 'hexcolor-add-to-font-lock)
(add-hook 'sass-mode-hook 'hexcolor-add-to-font-lock)
(add-hook 'less-css-mode-hook 'hexcolor-add-to-font-lock)
