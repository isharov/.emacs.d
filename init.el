;; package --- My init.el
;;; Commentary:
;;; Code:

(load "~/.emacs.d/helpers")
(load "~/.emacs.d/pkgs/tssh-tramp.el")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; (dolist (pkgdir (path/subdirs "~/.emacs.d/pkgs"))
;;   (add-to-list 'load-path pkgdir))

;; common editor customization
(setq
 c-default-style "linux"
 c-basic-offset 4
 require-final-newline t
 kill-whole-line t
 save-interprogram-paste-before-kill t  ;; Save the Clipboard Before Killing
 kill-do-not-save-duplicates t
 recentf-max-saved-items 5000
 enable-recursive-minibuffers t
 history-delete-duplicates t
 history-length 100
 scroll-preserve-screen-position 'always
 auto-save-default nil
 make-backup-files nil
 create-lockfiles nil
 ring-bell-function 'ignore
 max-mini-window-height 4
 window-combination-resize t  ;; Proportional Window Resizing
 ;; resize-mini-windows nil
 )
(setq-default tab-width 4
              indent-tabs-mode nil
              show-trailing-whitespace nil)

(add-hook 'after-change-major-mode-hook
          (lambda () (modify-syntax-entry ?_ "w"))) ; '_' is part of a word in all modes

(fset 'yes-or-no-p 'y-or-n-p) ; type y/n instead of yes/no
(blink-cursor-mode -1)

(load-file "~/.emacs.d/pkgs/russian-mac.el")
(setq default-input-method "russian-mac")

;; macbook keyboard modifications
(when (eq system-type 'darwin)
  (setq ns-function-modifier 'control   ; left-control
        mac-function-modifier 'control  ; left-control
        mac-option-modifier 'control    ; right-control
        mac-command-modifier 'meta
        mac-pass-command-to-system nil))

;; common modes
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(global-auto-revert-mode t)
(show-paren-mode 1)
(electric-pair-mode 1)
;(key-chord-mode 1)
(recentf-mode 1)
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))
(global-hl-line-mode 1)
(which-key-mode -1)    ; no popup of follow-up keys after a prefix
(global-eldoc-mode -1) ; no docs of the symbol at point in the echo area
(repeat-mode 1)        ; repeatable key sequences without re-pressing the prefix

;; Avoid performance issues in files with very long lines.
(global-so-long-mode 1)
;; These settings tell Emacs to assume left-to-right text everywhere
;; and skip the bidirectional parenthesis algorithm
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
;; Skip Fontification During Input
(setq redisplay-skip-fontification-on-input t)
;; Increase Process Output Buffer for LSP
(setq read-process-output-max (* 4 1024 1024)) ; 4MB

;; window navigation
(windmove-default-keybindings 'meta)
;(setq windmove-wrap-around t)
(winner-mode 1)

;; Always Open Emacs Buffers in the Current Active Window
(use-package single-window
  :vc (:url "https://github.com/jamescherti/single-window.el"
       :rev :newest)
  :config
  (single-window-mode 1)
  ;; hide the " single-window" mode-line lighter
  (setcdr (assq 'single-window-mode minor-mode-alist) '("")))

;; buffer moving
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; enable some commands
(put 'erase-buffer 'disabled nil)

;; auto-delete trailing whitespace
(add-hook 'write-file-hooks
          (lambda ()
            (when (not (derived-mode-p 'markdown-mode))  ; trailing whitespaces are meaningful in markdown
              (delete-trailing-whitespace)
              )))

;; prefer ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; minibuffer completion stack (vertico + orderless + marginalia + consult + embark)
(use-package vertico
  :ensure t
  :init (vertico-mode 1)
  :custom (vertico-cycle t))

;; helm-find-files-like path navigation: RET/TAB descend, DEL goes up a component
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init (marginalia-mode 1))

(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

;; hide M-x commands irrelevant to the current major mode
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; restore default bindings that helm had overridden (vertico now drives them)
(global-set-key (kbd "M-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "C-o") #'isharov/find-file-at-point)  ;; was open-line
;; (global-set-key (kbd "C-c o") #'open-line)  ;; open-line moved off C-o

;; consult: enhanced search / navigation commands
(use-package consult
  :ensure t
  :bind (("M-y"     . consult-yank-pop)      ;; was helm-show-kill-ring
         ("C-x b"   . consult-buffer)        ;; was helm-mini
         ("C-x C-r" . consult-recent-file)   ;; was helm-recentf
         ("C-x r b" . consult-bookmark)      ;; was helm-filtered-bookmarks
         ("M-i"     . isharov/consult-line)  ;; was helm-swoop (seed with selection)
         ("M-I"     . consult-line-multi)    ;; multi-buffer swoop
         ("M-g i"   . consult-imenu)         ;; jump to symbol/heading in buffer
         ("M-g I"   . consult-imenu-multi))  ;; ...across project buffers
  :custom (consult-line-start-from-top t))

;; consult-dir: jump the minibuffer to recent dirs, bookmarks, and TRAMP hosts
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)                 ;; was list-directory (rarely used)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))     ;; minibuffer-only; global C-x C-j stays dired-jump

;; project / directory ripgrep search (was helm-ag; project/ag lives in helpers.el)
(global-set-key (kbd "C-c g") 'project/ag)                      ;; project search
(global-set-key (kbd "C-c G")                                  ;; custom dir search (prompts for dir)
                (lambda () (interactive)
                  (consult-ripgrep '(4) (isharov/selection))))

;; project file finding / switching (was helm-ls-git; project-wide, no git requirement)
(global-set-key (kbd "C-c f") 'project-find-file)
(global-set-key (kbd "C-c p") 'project-switch-project)         ;; short alias for C-x p p

;; embark: contextual actions + export results to an editable buffer
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; wgrep: edit exported ripgrep results and write back to files (was helm-ag-edit)
(use-package wgrep
  :ensure t
  :custom (wgrep-auto-save-buffer t)
  :config
  ;; single-key `e' in an exported grep/ripgrep buffer -> editable wgrep
  ;; (then edit in place, C-c C-c to save all files, C-c C-k to abort)
  (with-eval-after-load 'grep
    (define-key grep-mode-map (kbd "e") #'wgrep-change-to-wgrep-mode)))

;; in-buffer completion: corfu + cape (was company)
(use-package corfu
  :ensure t
  :init (global-corfu-mode 1)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  :config
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(global-set-key (kbd "C-<tab>") #'completion-at-point)

;; dired
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(put 'dired-find-alternate-file 'disabled nil)
;; (setq dired-listing-switches "-la")
(add-hook 'dired-mode-hook
          (lambda ()
            ;; (dired-omit-mode)
            (define-key dired-mode-map (kbd "<return>")
              'dired-find-alternate-file) ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
            ))

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C->") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-S-c C-m") 'mc/mark-all-in-region)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-S-c C-SPC") 'set-rectangular-region-anchor)

;; fast cursor move
(use-package flash
  :commands (flash-jump flash-jump-continue flash-treesitter)
  :bind ("C-'" . flash-jump)
  :custom
  (flash-multi-window t)
  :init
  ;; Search integration (labels during C-s, /, ?)
  ;; (require 'flash-isearch)
  ;; (flash-isearch-mode 1)
  )

;; text selection
(global-set-key (kbd "S-M-SPC") 'isharov/select-current-line)

(use-package expreg
  :ensure t
  :bind (("C-M-SPC" . expreg-expand)
         ("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

(use-package treesit-sexp
  :vc (:url "https://github.com/alexispurslane/treesit-sexp"
       :rev :newest)
  :config
  (global-treesit-sexp-mode 1))

;; text moving
(global-set-key (kbd "<M-S-up>") 'move-text-up)
(global-set-key (kbd "<M-S-down>") 'move-text-down)

;; handy pairs
;; (global-set-key (kbd "M-[") 'insert-pair)
;; (global-set-key (kbd "M-{") 'insert-pair)
;; (global-set-key (kbd "M-\"") 'insert-pair)
;; (global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-)") 'delete-pair)

;; undo
;; (global-set-key (kbd "C-/") 'vundo)

;; project extra markers
(setq project-vc-extra-root-markers '(".project"))

;; magit-status as a project-switch-project action (C-x p p, then "m").
;; Also reachable directly as C-x p m inside a project.
(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "m") 'magit-status)
  (add-to-list 'project-switch-commands '(magit-status "Magit" ?m) t))

;; tree-sitter
;; NB: treesit-auto was tried here but its global-treesit-auto-mode made every
;; file-open (incl. consult preview) slow, so we keep the manual remaps instead.
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (js-json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)
        (rust-mode . rust-ts-mode)))

;; direnv
(direnv-mode)

;; flymake
(global-set-key (kbd "C-c e") 'consult-flymake) ;; navigable diagnostics list (was flymake-show-buffer-diagnostics)

;; tramp mode
(setq password-cache-expiry nil)
(setq tramp-histfile-override t)  ;; disable history file

;; docker
(global-set-key (kbd "C-c d") 'docker)

;; k8s
(require 'kubel)
(kubel-vterm-setup)
(setq kubel-log-tail-n 1000)

(defun k8s/zent-staging ()
  (interactive)
  (kubel-set-kubectl-config-file "~/.kube/zent.staging.config")
  (kubel-open "zent-staging" "staging" "pods"))

(defun k8s/zent-prod ()
  (interactive)
  (kubel-set-kubectl-config-file "~/.kube/zent.prod.config")
  (kubel-open "zent-production" "production" "pods"))

(defun k8s/clapp-staging ()
  (interactive)
  (kubel-set-kubectl-config-file "~/.kube/clapp.staging.config")
  (kubel-open "clapp-staging" "staging" "pods"))

;; (require 'kubed)
;; (keymap-global-set "C-c k" 'kubed-prefix-map)
;; (keymap-set kubed-prefix-map "k" #'kubed-transient)

;; gptel
;; (setq
;;  gptel-model 'phi4:latest
;;  gptel-backend (gptel-make-ollama "Ollama"
;;                  :host "localhost:11434"
;;                  :stream t
;;                  :models '(phi4:latest qwen2.5-coder:14b)))

;; aider
;; (use-package aidermacs
;;   :bind (("C-c a" . aidermacs-transient-menu))
;;   :config
;;   :custom
;;   (aidermacs-default-chat-mode 'architect)
;;   (aidermacs-show-diff-after-change nil)
;;   (aidermacs-default-model "openrouter/anthropic/claude-sonnet-4")
;;   (aidermacs-architect-model "openrouter/anthropic/claude-opus-4")
;;   )

;; eglot
(use-package eglot
  :ensure t
  :config (add-to-list 'eglot-server-programs
                       '((python-mode python-ts-mode) "basedpyright-langserver" "--stdio"))
  ;; :config (add-to-list 'eglot-server-programs
  ;;                      '((python-mode python-ts-mode) "ty" "server"))
  (setq eglot-report-progress nil)
  )

;; copilot
;; it has implicit editorconfig melpa dependency
;; (add-to-list 'load-path "~/.emacs.d/pkgs/copilot.el")
;; (require 'copilot)
;; (add-hook 'prog-mode-hook 'copilot-mode)
;; (define-key copilot-mode-map (kbd "M-C-<return>") 'copilot-accept-completion)

;; scala
;(require 'scala-mode2)
;(add-hook 'scala-mode-hook
;          (lambda ()
;            (local-set-key (kbd "C-c C-r")
;                           (lambda()
;                             (interactive)
;                             (buffer/create-send-region "*sbt-console*" "sbt console-quick")))))

;; xml
(setq nxml-child-indent 4 nxml-attribute-indent 4)

;; Javascript / JSON
;; npm install -g eslint eslint-plugin-react
;; /usr/local/bin/eslint -> /usr/local/lib/node_modules/eslint/bin/eslint.js --resolve-plugins-relative-to=/usr/local/lib/node_modules/ $@
(add-hook 'js-mode-hook
          (lambda ()
            (local-set-key (kbd "M-,") 'buffer/tag-region)
            (setq-default sgml-basic-offset 4)
            (setq indent-tabs-mode nil)
            ))
(setq json-ts-mode-indent-offset 4)

;; tsx
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; C++
(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-c C-t") 'isharov/toggle-source))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; using c++ mode for *.h files

;; python
;; M-x find-library RET python RET
(with-eval-after-load 'python
  (define-key python-ts-mode-map (kbd "C-c C-f")   ;; was python-eldoc-at-point
              (lambda ()
                (interactive)
                (buffer/shell-command "ruff format")
                (buffer/shell-command "ruff check --fix --unsafe-fixes")
                (revert-buffer t t t)
                )))
(add-hook 'python-ts-mode-hook
          (lambda ()
            ;; delete region if active else dedent
            (local-set-key (kbd "<backspace>")
                           (lambda ()
                             (interactive)
                             (if (use-region-p)
                                 (delete-backward-char 1)
                               (python-indent-dedent-line-backspace 1))
                             ))
            ))
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'python-ts-mode)
              (flymake-ruff-load))
            ))
;; (add-hook 'inferior-python-mode-hook
;;           (lambda ()
;;             (comint/turn-on-history)
;;             (define-key inferior-python-mode-map (kbd "M-r") 'consult-history)
;;             ))

;; go
;; go install golang.org/x/tools/gopls@latest
(add-hook 'go-mode-hook 'eglot-ensure)

;; rust
;; rustup component add rust-analyzer

;; git
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-diff-refine-hunk 'all)
;; single-window-mode forces every buffer into the current window, so the
;; commit diff would immediately replace COMMIT_EDITMSG. Skip it; C-c C-d
;; (magit-diff-while-committing) shows it on demand.
(setq magit-commit-show-diff nil)
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-o") 'magit-diff-visit-worktree-file-other-window))
;; (helm couldn't do completing-read-multiple, so magit octopus-merge selection
;;  used to be advised down to a single read here; vertico handles CRM natively.)

(global-diff-hl-mode)
(diff-hl-flydiff-mode)
; (diff-hl-margin-mode)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(setq diff-hl-disable-on-remote t)

;; svn
;; (require 'dsvn)
;; (global-set-key (kbd "C-c v s") 'isharov/svn-status)

;; color-theme
(when (window-system)
  (load-theme 'doom-one t)
  (theme/setup-font)
  )

;; shell
(add-hook 'shell-mode-hook 'comint/turn-on-history)
;(add-hook 'shell-mode-hook 'buffer-disable-undo)
;(add-hook 'shell-mode-hook (lambda () (goto-address-mode)))
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
(add-hook 'kill-buffer-hook 'comint-write-input-ring)
(add-hook 'kill-emacs-hook 'comint/write-input-ring-all-buffers)
;; consult-history reads comint-input-ring (was helm-comint-input-ring)
(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "M-r") 'consult-history))
(setq
 comint-input-ignoredups t           ; no duplicates in command history
 ;comint-completion-addsuffix t      ; insert space/slash after file completion
 comint-get-old-input (lambda () "") ; what to run when i press enter on a line above the current prompt
 comint-input-ring-size 5000         ; max shell history size
)
; in-buffer completion would stuck on slow tramp connection
(add-hook 'shell-mode-hook
          (lambda ()
            (if (file-remote-p (path/current-dir))
                (corfu-mode -1))))
;; vterm
(setq vterm-max-scrollback 20000)  ; max 100000
;; eat
(setq eat-term-name "xterm-256color")
;; ghostel
(use-package ghostel
  :ensure t
  :bind (:map ghostel-line-mode-map
         ("M-r" . ghostel/history)
         :map ghostel-mode-map
         ;; C-r is left to the terminal (fzf) in semi-char mode
         ("C-c M-r" . ghostel/history))
  :custom
  ;; scrollback in bytes
  (ghostel-max-scrollback (* 32 1024 1024)))

(global-set-key (kbd "C-c s") 'ghostel/new)  ;; new terminal (like C-u M-x ghostel)

(defun shell-arneb ()
  "Shortcut for arneb remote shell."
  (interactive)
  (let ((default-directory "/ssh:root@arneb#20002:/"))
    (shell "*shell-arneb*")))

(defun shell-bastion ()
  "Shortcut for bastion remote shell."
  (interactive)
  (let ((default-directory "/ssh:isharov@bastion.prd.clapp.clteam.io:/home/isharov/"))
    (shell "*shell-bastion*")))

(defun shell-gitlab ()
  "Shortcut for gitlab remote shell."
  (interactive)
  (let ((default-directory "/ssh:root@gitlab:/root/"))
    (shell "*shell-gitlab*")))

(defun shell-ob-analyzer-finland-01 ()
  "Shortcut for ob-analyzer-finland remote shell."
  (interactive)
  (let ((default-directory "/ssh:root@ob-analyzer-finland-01:/root/"))
    (shell "*shell-ob-analyzer-finland-01*")))

(defun shell-ob-analyzer-finland-02 ()
  "Shortcut for ob-analyzer-finland remote shell."
  (interactive)
  (let ((default-directory "/ssh:root@ob-analyzer-finland-02:/root/"))
    (shell "*shell-ob-analyzer-finland-02*")))

(defun shell-ob-analyzer-finland-03 ()
  "Shortcut for ob-analyzer-finland remote shell."
  (interactive)
  (let ((default-directory "/ssh:root@ob-analyzer-finland-03:/root/"))
    (shell "*shell-ob-analyzer-finland-03*")))

(defun shell-ob-analyzer-finland-04 ()
  "Shortcut for ob-analyzer-finland remote shell."
  (interactive)
  (let ((default-directory "/ssh:root@ob-analyzer-finland-04:/root/"))
    (shell "*shell-ob-analyzer-finland-04*")))

(defun shell-ob-analyzer-germany ()
  "Shortcut for ob-analyzer-germany remote shell."
  (interactive)
  (let ((default-directory "/ssh:root@ob-analyzer-germany:/root/"))
    (shell "*shell-ob-analyzer-germany*")))

(defun shell-stg-ob-analyzer-00 ()
  "Shortcut for stg-ob-analyzer remote shell."
  (interactive)
  (let ((default-directory "/ssh:root@stg-ob-analyzer-00:/root/"))
    (shell "*shell-stg-ob-analyzer-00*")))

(defun shell-synology ()
  "Shortcut for synology remote shell."
  (interactive)
  (let ((default-directory "/ssh:192.168.1.3:~"))
    (shell "*shell-synology*")))


;; org
(eval-after-load "org"
  '(progn
     (define-key org-mode-map [M-left] nil)
     (define-key org-mode-map [M-right] nil)
     (define-key org-mode-map [M-up] nil)
     (define-key org-mode-map [M-down] nil)
     (define-key org-mode-map [C-left] 'org-metaleft)
     (define-key org-mode-map [C-right] 'org-metaright)
     (define-key org-mode-map [C-up] 'org-metaup)
     (define-key org-mode-map [C-down] 'org-metadown)
     ))

;; mermaid
;; npm install -g @mermaid-js/mermaid-cli
;; (setq mermaid-mmdc-location "docker")
;; (setq mermaid-flags "run --rm -v /tmp:/tmp ghcr.io/mermaid-js/mermaid-cli/mermaid-cli:9.1.6")
(setq mermaid-flags "-s 3")
;; (setq mermaid-output-format ".svg")

;; restclient.el
(require 'restclient)
(setq restclient-inhibit-cookies t)  ;; enforce explicit cookies
;(setq tls-program '("gnutls-cli --insecure --x509cafile %t -p %p %h" "gnutls-cli --insecure --x509cafile %t -p %p %h --protocols ssl3"))
;(custom-reevaluate-setting 'tls-program)

;; setup default desktop
(setq inhibit-startup-screen t)
(toggle-frame-maximized)
(split-window-vertically)
(split-window-horizontally)
(windmove-down)
(split-window-horizontally)
(windmove-up)

(let ((default-directory (or (getenv "EMACS_DEFAULT_DIRECTORY") "~/dev")))
  (ghostel 1)
  (ghostel 2)
  )

(when (eq system-type 'darwin)
  (setq delete-by-moving-to-trash t)
  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash.
     When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash") nil 0 nil file))
  )

(provide 'init)
;;; init.el ends here
