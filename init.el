;; package --- My init.el
;;; Commentary:
;;; Code:

(load "~/.emacs.d/helpers")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

(dolist (pkgdir (path/subdirs "~/.emacs.d/pkgs"))
  (add-to-list 'load-path pkgdir))

;; common editor customization
(setq c-default-style "linux"
      c-basic-offset 4
      require-final-newline t
      kill-whole-line t
      recentf-max-saved-items 5000
      enable-recursive-minibuffers t
      history-delete-duplicates t
      history-length 100
      scroll-preserve-screen-position 'always
      auto-save-default nil
      make-backup-files nil
      create-lockfiles nil
      ring-bell-function 'ignore)
(setq-default tab-width 4
              indent-tabs-mode nil
              show-trailing-whitespace nil)
(add-to-list 'default-frame-alist '(font . "Fira Code 12"))
; (add-to-list 'default-frame-alist '(font . "Victor Mono"))
(fset 'yes-or-no-p 'y-or-n-p) ; type y/n instead of yes/no
(blink-cursor-mode -1)

(mac-auto-operator-composition-mode)  ; ligatures support

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
(global-eldoc-mode 0) ; It works bad with Python for now
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

;; Avoid performance issues in files with very long lines.
(global-so-long-mode 1)
(setq bidi-inhibit-bpa t)

;; window navigation
(windmove-default-keybindings 'meta)
;(setq windmove-wrap-around t)
(winner-mode 1)

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

;; helm mode
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(define-key minibuffer-local-map (kbd "M-r") 'helm-minibuffer-history)

(setq helm-split-window-inside-p t)
(when (executable-find "curl")
  (setq helm-net-prefer-curl t))
(helm-mode 1)

;; helm-swoop
(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
(setq helm-swoop-split-with-multiple-windows t)

;; helm-ag
(global-set-key (kbd "C-c g") 'project/ag)
(setq helm-ag-base-command "ag --hidden --nocolor --nogroup --ignore-case --ignore=.git --ignore=.svn"
      helm-ag-insert-at-point 'symbol)

;; helm-ls-git
(require 'helm-ls-git)
(global-set-key (kbd "C-c f") 'helm-browse-project)
(global-set-key (kbd "C-x r p") 'helm-projects-history)

;; company
(add-hook 'after-init-hook 'global-company-mode)

;; dired
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda ()
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
(avy-setup-default)
(global-set-key (kbd "C-'") 'avy-goto-char-timer)
(setq avy-all-windows nil) ; this window only

;; text selection
(require 'expand-region)
(global-set-key (kbd "C-M-SPC") 'er/expand-region)
(global-set-key (kbd "S-M-SPC") 'isharov/select-current-line)

;; semantic navigation
(require 'smart-forward)
(global-set-key (kbd "C-<up>") 'smart-up)
(global-set-key (kbd "C-<down>") 'smart-down)
(global-set-key (kbd "C-<left>") 'smart-backward)
(global-set-key (kbd "C-<right>") 'smart-forward)

;; text moving
(global-set-key (kbd "<M-S-up>") 'move-text-up)
(global-set-key (kbd "<M-S-down>") 'move-text-down)

;; handy pairs
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-)") 'delete-pair)

;; lsp
(require 'lsp-mode)
(setq lsp-prefer-flymake nil)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; spell checking
(global-set-key (kbd "C-c s") 'isharov/toggle-flyspell)

;; flycheck
(global-flycheck-mode)
(global-set-key (kbd "C-c c") 'flycheck-mode)
(global-set-key (kbd "C-c C-v") 'flycheck-list-errors)
(add-hook 'flycheck-mode-hook
          (lambda ()
            (setq flycheck-checker-error-threshold nil
                  flycheck-indication-mode nil)
            ))

;; tramp mode
(setq password-cache-expiry nil)

;; docker
(global-set-key (kbd "C-c d") 'docker)

;; scala
;(require 'scala-mode2)
;(add-hook 'scala-mode-hook
;          (lambda ()
;            (local-set-key (kbd "C-c C-r")
;                           (lambda()
;                             (interactive)
;                             (buffer/create-send-region "*sbt-console*" "sbt console-quick")))))

;; js
;; npm install -g eslint eslint-plugin-react
;; /usr/local/bin/eslint -> /usr/local/lib/node_modules/eslint/bin/eslint.js --resolve-plugins-relative-to=/usr/local/lib/node_modules/ $@
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-hook 'rjsx-mode-hook
          (lambda ()
            (local-set-key (kbd "M-,") 'buffer/tag-region)
            (setq-default sgml-basic-offset 4)
            ))

;; C++
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-x t") 'isharov/toggle-source)
            (modify-syntax-entry ?_ "w") ; now '_' is not considered a word-delimiter
            ))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; using c++ mode for *.h files

;; python
;; pip install -U python-language-server
;(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'python-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-c C-v"))
            (modify-syntax-entry ?_ "w") ; now '_' is not considered a word-delimiter
            (local-set-key (kbd "C-c C-b") (lambda () (interactive) (buffer/shell-command "black")))
            (local-set-key (kbd "C-c C-s") (lambda () (interactive) (buffer/shell-command "isort")))
            ))
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (comint/turn-on-history)
            (define-key inferior-python-mode-map (kbd "M-r") 'helm-comint-input-ring)
            ))
(add-hook 'flycheck-mode-hook
          (lambda ()
            (setq flycheck-python-flake8-executable "python3"
                  flycheck-python-pylint-executable "python3"
                  flycheck-python-pycompile-executable "python3"
                  flycheck-flake8rc "~/.config/flake8")
            ))

;; go
;; go get golang.org/x/tools/gopls@latest
; (add-hook 'go-mode-hook #'lsp-deferred)

;; git
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-diff-refine-hunk t)

(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; svn
(require 'dsvn)
(global-set-key (kbd "C-c v s") 'isharov/svn-status)

;; color-theme
(when (window-system)
  (load-theme 'spacemacs-dark t)
  (set-face-italic 'font-lock-comment-face 1)
  )

;; shell
(add-hook 'shell-mode-hook 'comint/turn-on-history)
;(add-hook 'shell-mode-hook 'buffer-disable-undo)
;(add-hook 'shell-mode-hook (lambda () (goto-address-mode)))
;(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
(add-hook 'kill-buffer-hook 'comint-write-input-ring)
(add-hook 'kill-emacs-hook 'comint/write-input-ring-all-buffers)
(define-key shell-mode-map (kbd "M-r") 'helm-comint-input-ring)
(setq
 comint-input-ignoredups t           ; no duplicates in command history
 ;comint-completion-addsuffix t      ; insert space/slash after file completion
 comint-get-old-input (lambda () "") ; what to run when i press enter on a line above the current prompt
 comint-input-ring-size 5000         ; max shell history size
)
; company completion would stuck on slow tramp connection
(add-hook 'shell-mode-hook
          (lambda ()
            (if (file-remote-p (path/current-dir))
                (company-mode -1))))

(defun shell-acamar ()
  "Shortcut for veesp-acamar remote shell."
  (interactive)
  (let ((default-directory "/scp:root@veesp-acamar:/"))
    (shell "*shell-acamar*")))

(defun shell-vlab ()
  "Shortcut for vLab remote shell."
  (interactive)
  (let ((default-directory "/scp:isharov@vlab-dev:/home/isharov/dev/aucore/AuCore/app/"))
    (shell "*shell-vlab*")))

(defun shell-appliance ()
  "Shortcut for local appliance VM shell."
  (interactive)
  (let ((default-directory "/scp:root@appliance-local:/"))
    (shell "*shell-appliance*")))

(defun shell-arm64 ()
  "Shortcut for debian-arm64 remote shell."
  (interactive)
  (let ((default-directory "/scp:debian@arm64-dev:/home/debian/dev/aucore/AuCore/app/"))
    (shell "*shell-arm64*")))

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

;; restclient.el
(require 'restclient)
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
(find-file "~/dev/authasas/aucore/notes.org")
(magit-status)
(let ((default-directory "~/dev/authasas/aucore/AuCore/app/"))
  (shell "*shell<1>*")
  (shell "*shell<2>*")
  )
(execute-kbd-macro "\C-m")
(buf-move-left)
(toggle-frame-fullscreen)

(provide 'init)
;;; init.el ends here
