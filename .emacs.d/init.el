(require 'cask "~/.cask/cask.el")

(cask-initialize)
(require 'pallet)
(pallet-mode t)
(exec-path-from-shell-initialize)
(guru-global-mode +1)

(load "~/.emacs.d/mu4e.el")
(load "~/.emacs.d/ruby.el")
(load "~/.emacs.d/prodigy.el")
(load "~/.emacs.d/slack.el")


;;------- Enviroment ---------

(column-number-mode)
(setq fill-column 80)
(setq-default fill-column 72)

;; Notifications
(add-to-list 'erc-modules 'notifications)

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
(setq make-backup-files nil)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Mac specific stuff
(when (eq system-type 'darwin)
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  ;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char)
  (menu-bar-mode 1))

;; Neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; Flyspell
(setq ispell-list-command "--list")
(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "pt_BR") "english" "pt_BR")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

(global-set-key (kbd "<f5>") 'fd-switch-dictionary)
(global-set-key (kbd "<f6>") 'ispell-word)

;;-------- Interface --------

;; no bars
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; setting up a color theme
(load-theme 'monokai t)

;; show line numbers
(require 'linum)
(global-linum-mode 0)
(global-set-key (kbd "C-c C-n") 'linum-mode)

;; hightlight currentline
(hl-line-mode)

;; Always do syntax highlighting
(global-font-lock-mode 1)

;; Also highlight parens
(setq show-paren-delay 0 show-paren-style 'parenthesis)
(show-paren-mode 1)
(smartparens-global-mode t)

;;
(require 'corral)
(global-set-key (kbd "M-9") 'corral-parentheses-backward)
(global-set-key (kbd "M-0") 'corral-parentheses-forward)
(global-set-key (kbd "M-{") 'corral-braces-backward)
(global-set-key (kbd "M-}") 'corral-braces-forward)
(global-set-key (kbd "M-\"") 'corral-double-quotes-backward)

;; highlight mark region
(transient-mark-mode 1)

;; respecting boundaries
(require 'fill-column-indicator)
(fci-mode)
(setq fci-rule-width 1)
(setq fci-rule-color "#333333")

;; scroll smoothly
(setq scroll-conservatively 10000)

;; Nyan-mode
(nyan-mode t)

;; Transparency
;; (set-frame-parameter (selected-frame) 'alpha '(100 50))
;; (add-to-list 'default-frame-alist '(alpha 100 50))

;;-------- Modes --------
(global-aggressive-indent-mode 1)

;; Css
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level 4)

;; Sass
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))


;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-hook 'markdown-mode-hook '(lambda() (flyspell-mode)))

;; Web Mode
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

;; Slim-mode
(require 'slim-mode)

;; Javascript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-consistent-level-indent-inner-bracket-p 1)
(setq js2-pretty-multiline-decl-indentation-p 1)
(setq js2-bounce-indent-p t)
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)
(add-hook 'js-mode-hook 'my-paredit-nonlisp)

;; Pomodoro
(require 'pomodoro)
(pomodoro-add-to-mode-line)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-ignore-case nil)
(setq ac-dwim 2)
(ac-config-default)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)





;;-------- Keybinds --------
(require 'bind-key)

;; change window
(global-set-key [C-tab] 'other-window)

;; comments
(global-set-key [(ctrl c) (c)] 'comment-region)
(global-set-key [(ctrl c) (d)] 'uncomment-region)

;; delete trailing whitespace
(global-set-key [(ctrl x) (w)] 'delete-trailing-whitespace)

;; buffer
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

;; Navegation
(global-set-key (kbd "M-g") 'goto-line)

;; Sort
(global-set-key (kbd "C-c s") 'sort-lines)

;; Cool Enter
(bind-key "C-j" 'newline-and-indent)

;; Multiple Cursors
(global-set-key (kbd "C-c C-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; highlight indentation column
(global-set-key (kbd "M-1") 'highlight-indentation-current-column-mode)

;; scrolling without changing the cursor
(global-set-key [(meta n)] '(lambda () (interactive) (scroll-up 1)))
(global-set-key [(meta p)] '(lambda () (interactive) (scroll-down 1)))

;; scrolling other window
(global-set-key [(meta j)] '(lambda () (interactive) (scroll-other-window 1)))
(global-set-key [(meta k)] '(lambda () (interactive) (scroll-other-window -1)))

;; Fiplr
(bind-key (kbd "C-c f") 'fiplr-find-file)
(setq fiplr-ignored-globs '((directories (".git" ".svn"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" "public/*" "tmp/*" "vendor" "bin" "docs" "log" "script"))))

;; Projectile
;; (bind-key (kbd "C-c C-f") 'projectile-find-file)
(bind-key (kbd "C-c C-a") 'helm-projectile-ack)

;; Rinari
(bind-key (kbd "C-c m") 'rinari-find-model)
(bind-key (kbd "C-c l") 'rinari-find-controller)
(bind-key (kbd "C-c v") 'rinari-find-view)
(bind-key (kbd "C-c k") 'rinari-console)


;;-------- HOOKS ----------

;; Remove whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enabling the server mode by default
(server-mode)


(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
