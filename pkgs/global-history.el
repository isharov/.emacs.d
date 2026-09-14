;;; global-history.el --- One shell history across ghostel, comint and nested shells  -*- lexical-binding: t; -*-

;;; Commentary:

;; comint keeps an input ring per buffer; ghostel is a real pty and keeps
;; none.  `ghostel-shell-history' asks the buffer's own shell (zsh: "fc -R;
;; fc -lnr 1"), which covers the running session and TRAMP hosts -- but not
;; a shell reached through `ssh' or `docker exec' typed *inside* the
;; terminal, whose history lives on the far side.  No package does this:
;; every one of them hooks `comint-send-input', and a pty never goes there.
;;
;; So record submitted lines here instead, from every ghostel and comint
;; buffer, into one savehist-backed list.  The capture point is Return, and
;; the prompt/input boundary comes from `ghostel-input-start-point': OSC 133
;; markers when the shell emits them, `ghostel-prompt-regexp' when it does
;; not -- which is exactly the nested ssh/docker case.  With neither, that
;; function returns the cursor position itself, so the captured region is
;; empty and nothing is recorded: junk from `cat', a pager, or an unechoed
;; password prompt is skipped for free.
;;
;; `history/pick' reads this list *and* the buffer's own history, so a local
;; terminal still offers everything zsh wrote from Ghostty or tmux.
;;
;; Usage, from init.el:
;;
;;   (load "~/.emacs.d/pkgs/global-history.el")   ; before `savehist-mode'
;;   (history/global-setup)
;;
;; Load order matters in one direction only: `history/global' must be
;; defined before `savehist-mode' turns on, because that is when savehist
;; loads its file and setqs the saved value back.  Loading this file at the
;; top of init.el, with the other pkgs, satisfies that.  `history/global-setup'
;; itself can run at any later point.

;;; Code:

(require 'comint)
(require 'seq)
(require 'subr-x)

(declare-function ghostel-alt-screen-p "ghostel")
(declare-function ghostel-input-start-point "ghostel")
(declare-function ghostel-send-string "ghostel")
(declare-function ghostel-shell-history "ghostel-shell")
(declare-function ghostel--line-mode-input-text "ghostel-line-mode")
(declare-function ghostel--line-mode-replace-input "ghostel-line-mode")
(defvar ghostel--input-mode)

;;; Storage

(defcustom history/global-size 10000
  "Maximum number of entries kept in `history/global'."
  :type 'integer
  :group 'shell)

(defcustom history/global-ignore-regexp "\\`\\s-"
  "Input matching this regexp is not recorded.
The default drops space-prefixed lines, like zsh's `HIST_IGNORE_SPACE',
so a secret can be kept out of the history by typing a leading space."
  :type '(choice (const :tag "Record everything" nil) regexp)
  :group 'shell)

(defcustom history/global-max-length 1000
  "Input longer than this many characters is not recorded.
A command line this long is in practice a paste rather than something
typed, and the ones in a shell history tend to be `curl' invocations
carrying cookies and auth tokens -- content worth keeping out of a file
that outlives the session.  Nil records input of any length."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'shell)

(defvar history/global nil
  "Global shell history, newest first.
Persisted by `savehist', which `history/global-setup' registers it with.
The restore survives this `defvar' because savehist loads its file when
`savehist-mode' turns on, which init.el does after loading helpers.el.")

(defun history/global-add (input)
  "Record INPUT in `history/global'.
`add-to-history' does the rest: it drops empty input and a repeat of the
previous entry, honours `history-delete-duplicates', and truncates to the
`history-length' property that `history/global-setup' puts on the symbol."
  (unless (or (and history/global-ignore-regexp
                   (string-match-p history/global-ignore-regexp input))
              (and history/global-max-length
                   (> (length input) history/global-max-length)))
    (add-to-history 'history/global (string-trim input))))

;;; Capture

(defun history/ghostel--submitted-input ()
  "Return the text sitting at the current ghostel prompt, or nil.
Nil while a fullscreen TUI owns the screen, and nil when no prompt can
be located -- see the commentary above."
  (when (and (derived-mode-p 'ghostel-mode)
             (not (ghostel-alt-screen-p)))
    (let ((start (ghostel-input-start-point)))
      (when start
        ;; `ghostel-prompt-regexp' ends in `[ \u00a0]*\=', so it swallows
        ;; every space after the prompt sigil -- including one the user typed
        ;; to hide the command.  Hand back all but the prompt's own separator
        ;; so `history/global-ignore-regexp' can still see it.  Under OSC 133
        ;; nothing is over-consumed and this is a no-op.
        (let* ((eaten (save-excursion
                        (goto-char start)
                        (- (skip-chars-backward " \u00a0"))))
               (start (- start (max 0 (1- eaten))))
               (end (save-excursion (goto-char start) (line-end-position))))
          (when (> end start)
            (buffer-substring-no-properties start end)))))))

(defun history/ghostel-record-return (&rest _)
  "Record the prompt line when Return is what is being sent to the pty.
Advice on `ghostel--send-event', which handles every key in char and
semi-char mode, so it has to filter for Return itself."
  (when (memq (event-basic-type last-command-event) '(?\r return))
    (when-let* ((input (history/ghostel--submitted-input)))
      (history/global-add input))))

(defun history/ghostel-record-line-mode (&rest _)
  "Record the line-mode input.  Advice on `ghostel-line-mode-send'.
Line mode owns its input in the buffer, so it never reaches
`ghostel--send-event' as a Return."
  (history/global-add (ghostel--line-mode-input-text)))

(defun history/comint-record (input)
  "Record INPUT, submitted in a comint buffer.
For `comint-input-filter-functions', which runs in every comint-derived
mode, so remote `shell' buffers land in the global history too.  Password
prompts bypass this hook -- `comint-send-invisible' calls
`comint-input-sender' directly -- so they cannot leak in."
  (history/global-add input))

(defun history/global-setup ()
  "Start recording every shell line into the global history."
  (require 'savehist)
  ;; `history-length' is 100 globally; this one wants far more.
  (put 'history/global 'history-length history/global-size)
  (add-to-list 'savehist-additional-variables 'history/global)
  (advice-add 'ghostel--send-event :before #'history/ghostel-record-return)
  (with-eval-after-load 'ghostel-line-mode
    (advice-add 'ghostel-line-mode-send :before #'history/ghostel-record-line-mode))
  (add-hook 'comint-input-filter-functions #'history/comint-record))

;;; Backfill

(defun history/global--strip-timestamp (entry)
  "Strip zsh EXTENDED_HISTORY metadata from ENTRY.
With that option set zsh writes \": <seconds>:<elapsed>;<command>\"
instead of the bare command."
  (if (string-match "\\`: [0-9]+:[0-9]+;" entry)
      (substring entry (match-end 0))
    entry))

(defun history/global--file-entries (file)
  "Return FILE's history entries, newest first.
comint history files and a shell HISTFILE are both oldest-first with one
entry per line; in a zsh file a trailing backslash continues the entry
onto the next line, which is how it stores a multi-line command.  The
per-entry metadata the shells can be configured to write -- zsh's
EXTENDED_HISTORY prefix, bash's HISTTIMEFORMAT comment line -- is
dropped, so a remote host with either option set still parses."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)          ; auto-detect: histfiles are not always clean UTF-8
      (let (entries pending)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ;; bash with HISTTIMEFORMAT stamps a "#<seconds>" line of its
             ;; own before each entry.
             ((and (null pending) (string-match-p "\\`#[0-9]+\\'" line)))
             ((string-suffix-p "\\" line)
              (setq pending (concat pending (substring line 0 -1) "\n")))
             (t
              (push (history/global--strip-timestamp (concat pending line))
                    entries)                         ; reading forward ends newest first
              (setq pending nil))))
          (forward-line 1))
        (when pending (push pending entries))
        entries))))

(defun history/global-backfill (&optional include-histfile)
  "Seed `history/global' from the history files that predate it.
Reads the per-buffer comint rings `comint/turn-on-history' leaves in
`user-emacs-directory' -- which carry every remote `shell' session, and
are reachable no other way once the buffer is gone.

With a prefix argument INCLUDE-HISTFILE, also read the shell's own
HISTFILE.  That one is redundant in a local terminal, where
`history/candidates' already queries the live shell, but it does make
those commands reachable from a remote buffer.

Existing entries keep their position and backfilled ones go behind them,
deduplicated and truncated to `history/global-size'."
  (interactive "P")
  (let* ((files (append
                 (file-expand-wildcards
                  (expand-file-name ".inferior-*-history" user-emacs-directory))
                 (when include-histfile
                   (list (or (getenv "HISTFILE")
                             (expand-file-name "~/.zsh_history"))))))
         (found (mapcan #'history/global--file-entries files))
         ;; Same gate as live capture, so the cap and the ignore regexp
         ;; apply to what comes off disk too.
         (clean (seq-filter
                 (lambda (e)
                   (and (not (string-empty-p e))
                        (not (and history/global-ignore-regexp
                                  (string-match-p history/global-ignore-regexp e)))
                        (or (null history/global-max-length)
                            (<= (length e) history/global-max-length))))
                 (mapcar #'string-trim found)))
         (before (length history/global)))
    ;; Trailing nil so `delete-dups' cannot splice `history/global' itself.
    (setq history/global
          (seq-take (delete-dups (append history/global clean nil))
                    history/global-size))
    (when (bound-and-true-p savehist-mode) (savehist-save))
    (message "Backfill: %d entries in %d file(s), %d new; history %d -> %d"
             (length clean) (length files)
             (- (length history/global) before)
             before (length history/global))))

;;; Retrieval

(defcustom history/shell-history-files
  '("~/.zsh_history" "~/.bash_history" "~/.histfile" "~/.ash_history")
  "History files tried, in order, when the shell cannot answer for itself.
Read relative to the terminal's host, so on a TRAMP buffer these are
the remote user's files.  The first readable one wins."
  :type '(repeat string)
  :group 'shell)

(defun history/shell--histfile-entries ()
  "Entries from the first readable `history/shell-history-files', or nil.
Resolved on the buffer's host: `insert-file-contents' goes through
TRAMP for a remote `default-directory', so no remote process is
involved -- only what the shell has already flushed to disk."
  (let ((host (or (file-remote-p default-directory) "")))
    (seq-some (lambda (name)
                (let ((file (concat host name)))
                  (and (file-readable-p file)
                       (history/global--file-entries file))))
              history/shell-history-files)))

(defun history/shell-history ()
  "The current buffer's own shell history, newest first, or nil.
For a terminal, `ghostel-shell-history' is the better answer: it asks
the live shell, so it has the running session's commands and not just
what has been flushed to disk.  It needs ghostel to have recognized the
shell though, and on a remote host that runs through `getent passwd'
and then an interactive shell over TRAMP -- both of which come back
empty-handed on a trimmed-down image, leaving `M-r' with nothing but
the global history.  Fall back to the history file, which needs
neither."
  (cond
   ((derived-mode-p 'ghostel-mode)
    (let (failure)
      (or (condition-case err
              (ghostel-shell-history)
            (error (setq failure err) nil))
          (history/shell--histfile-entries)
          (progn
            (when failure
              (message "No shell history here: %s"
                       (error-message-string failure)))
            nil))))
   ((and (derived-mode-p 'comint-mode) (ring-p comint-input-ring))
    (ring-elements comint-input-ring))))

(defun history/candidates (&optional global-only)
  "Merged history for the current buffer, newest first, deduplicated.
The buffer's own history comes first -- see `history/shell-history' --
then everything ever recorded globally.  GLOBAL-ONLY skips the buffer's
own, which for a terminal means skipping a subprocess (and, on TRAMP, a
round trip)."
  (let ((local (unless global-only (history/shell-history))))
    ;; Trailing nil forces `append' to copy: `delete-dups' is destructive
    ;; and would otherwise cut entries out of `history/global' itself.
    (delete-dups (append local history/global nil))))

(defun history/pick (&optional global-only)
  "Pick a command from the shell history and put it at the prompt.
The command is typed but not sent, so it can still be edited.  With a
prefix argument GLOBAL-ONLY, offer only the global history."
  (interactive "P")
  (let* ((cmds (history/candidates global-only))
         (cmd (completing-read
               "History: "
               (lambda (str pred action)
                 (if (eq action 'metadata)
                     '(metadata (display-sort-function . identity))
                   (complete-with-action action cmds str pred)))
               nil t)))
    (cond
     ((eq (bound-and-true-p ghostel--input-mode) 'line)
      (ghostel--line-mode-replace-input cmd))
     ((derived-mode-p 'ghostel-mode) (ghostel-send-string cmd))
     ((derived-mode-p 'comint-mode)
      (delete-region (comint-line-beginning-position) (point-max))
      (insert cmd))
     (t (insert cmd)))))

(defalias 'ghostel/history #'history/pick)

(provide 'global-history)

;;; global-history.el ends here
