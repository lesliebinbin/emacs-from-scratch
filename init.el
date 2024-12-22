(setq inhibit-startup-message t
      use-package-always-ensure t
      make-backup-files nil
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/"))
)
(scroll-bar-mode nil)
(tool-bar-mode nil)
(tooltip-mode nil)
(set-fringe-mode 10)

(set-face-attribute 'default nil :font "Lucida Console" :height 180)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'package)


(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)


(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind  (:map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode t))

;; Swiper configuration
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper) ;; Explicitly bind here
         ("C-r" . swiper-backward))
  :config
  ;; Additional swiper settings if needed
  )

(use-package counsel
    :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c k" . counsel-rg)
	 ("C-x b" . counsel-ibuffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)
	 )
)

    


(use-package compat)

(use-package nerd-icons)

(use-package doom-modeline
  :after compat
  :init (doom-modeline-mode t)
  :custom ((doom-modeline-height 15)))


(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))
  

(use-package ivy-rich
  :init (ivy-rich-mode t))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  )

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;;just for demostration, not in used
;;(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; just for demostration, not in used
;;(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

(use-package general
:config
(general-create-definer leslie/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")
)

(use-package evil
  :init
  (setq
   ;; evil-want-integration t		
   evil-want-keybinding nil
   ;; evil-want-C-u-scroll t
   ;; evil-want-C-i-jump nil
   )
  :config
  (evil-mode t)
  ;;(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;;(evil-set-initial-state 'messages-buffer-mode 'normal)
  ;;(evil-set-initial-state 'dashboard-mode 'normal)
  )

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(leslie/leader-keys
  "t" '(:ignore t :which-key "toggle")
  "tt" '(counsel-load-theme :which-key "choose theme"))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(leslie/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :init (projectile-mode +1)
  :custom ((projectile-completion-system 'ivy))
  :config
  (leslie/leader-keys
    "p"  '(:ignore t :which-key "projectile")  ;; Create a "projectile" prefix under SPC
    "pp" '(projectile-command-map :which-key "projectile commands"))
  :init
  (setq projectile-project-search-path '("~/Desktop/codings")
	projectile-switch-project-action #'projectile-dired)
)

(use-package counsel-projectile
  :config (counsel-projectile-mode))


(use-package magit
  :custom
  ((magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
  :config
  (leslie/leader-keys
    "g" '(:ignore t :which-key "magit")
    "gs" '(magit-status :which-key "status")
    )
  )

(use-package iedit)

(use-package evil-iedit-state
  :after iedit
  :config
  ;; Automatically activate evil-iedit-state in iedit-mode
  (add-hook 'iedit-mode-hook 'evil-iedit-state)
  (leslie/leader-keys
    "s"  '(:ignore t :which-key "iedit")  ;; Create an "iedit" prefix under SPC s
    "se" '(iedit-mode :which-key "iedit-mode")
    )
  )

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package wgrep)


(toggle-frame-fullscreen)
