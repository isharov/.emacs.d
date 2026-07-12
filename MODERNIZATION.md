# Modernizing the Emacs setup: Helm → Vertico/Consult, company → corfu

Status: **plan** (nothing applied yet). Target: Emacs 30.2.

Up front: **every Helm feature currently in use maps cleanly to the modern
stack**, and migrating lets us *delete* two workarounds we currently carry
(the `helm-ag` read-only advice and the `magit-merge` CRM override). The
`company` swap is orthogonal but included here as its own phase.

---

## 1. Target stacks

### Minibuffer completion (replaces Helm)

The modern equivalent of Helm isn't one package — it's a few composable ones,
each doing one job:

| Package | Job | Replaces |
|---|---|---|
| **vertico** | vertical completion UI in the minibuffer | Helm's window/UI, `helm-mode` |
| **orderless** | space-separated / fuzzy matching | Helm's fuzzy matching |
| **marginalia** | annotations (docstrings, keybindings, file info) | Helm's rich lines |
| **consult** | enhanced commands (`consult-line`, `consult-ripgrep`, …) | `helm-swoop`, `helm-ag`, `helm-mini`, etc. |
| **embark** | context actions + export to editable buffers | Helm action menu, `helm-ag-edit` |
| **embark-consult** | glue between the two | — |
| **wgrep** | edit ripgrep results and write back to files | `helm-ag-edit` |
| **savehist** (built-in) | persist minibuffer history | Helm's history |

### In-buffer completion (replaces company)

The sibling stack, same minimalist philosophy, native `completion-at-point`:

| Package | Job | Replaces |
|---|---|---|
| **corfu** | in-buffer completion popup | `company` UI |
| **cape** | `completion-at-point` backends (dabbrev, file, keyword, …) | company backends |
| **corfu-popupinfo** (bundled) | docstring/signature popup next to candidates | `company` doc buffer |
| **kind-icon** (optional) | icons per candidate kind | company icons |
| **corfu-terminal** (optional) | corfu in `-nw`/TTY frames | — (only if used in a terminal) |

Both stacks are lighter and faster than what they replace, and `eglot` +
tree-sitter (already in use) feed `corfu` and `consult` natively through
`completion-at-point` / `xref` / `imenu`.

---

## 2. Feature parity — every current binding preserved

### Helm commands

| Binding | Today (Helm) | Modern replacement | Notes |
|---|---|---|---|
| `M-x` | `helm-M-x` | `execute-extended-command` (vertico + marginalia) | shows keys + docstrings |
| `M-y` | `helm-show-kill-ring` | `consult-yank-pop` | live preview |
| `C-x b` | `helm-mini` | `consult-buffer` | buffers + recentf + bookmarks in one |
| `C-x C-f` | `helm-find-files` | `find-file` + `vertico-directory` | see soft-gap note below |
| `C-x C-r` | `helm-recentf` | `consult-recent-file` | |
| `C-x r b` | `helm-filtered-bookmarks` | `consult-bookmark` | |
| `M-i` | `helm-swoop` | `consult-line` (+ `consult-line-multi`) | direct analogue, with preview |
| `C-c g` | `project/ag` | `consult-ripgrep` (project root) | thin wrapper keeps `project/root` logic |
| `C-c G` | `helm-do-ag` | `consult-ripgrep` (prompt for dir) | |
| `C-c f` | `helm-browse-project` | `project-find-file` or `consult-ls-git` | git-tracked file listing |
| `C-x r p` | `helm-projects-history` | `project-switch-project` (built-in) | project.el tracks the list |
| `M-r` (shell) | `helm-comint-input-ring` | `consult-history` | reads `comint-input-ring`; see note below |
| everywhere | `helm-mode` generic `completing-read` | vertico (global) | one UI for all prompts |

**Editing search results** (`helm-ag-edit` muscle memory): with ripgrep results
open, `embark-export` dumps them into a `grep-mode` buffer, then `wgrep` makes it
editable — edit matches inline, `C-c C-c` writes back to all files. Strictly more
capable than `helm-ag-edit`.

**Comint history search** (`M-r`): `consult-history` is purpose-built for this.
In a comint buffer it reads the existing `comint-input-ring`, so all current
history plumbing keeps working untouched — the per-shell
`~/.emacs.d/.inferior-*-history` files (`helpers.el:204`),
`comint-input-ring-size 5000`, `comint-input-ignoredups`, and the
`comint-write-input-ring` on kill/exit. It just *reads* that ring:

```elisp
(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "M-r") #'consult-history))
```

This replaces `init.el:441` and lets us **retire the vendored
`pkgs/helm-comint/` package** — deleting the `load-path` add and
`(load "helm-comint.el")` at `init.el:433-434`, which also resolves the in-code
`TODO: is it deprecated? What's the alternative` on that line.

Caveat: `consult-history` works in comint-derived buffers (`shell`,
`inferior-python`, eshell, minibuffer), not `vterm`. The `M-r` binding lives in
`shell-mode-map` and every remote shell (`shell-arneb`, `shell-bastion`, …) uses
`shell`, so this is fully covered.

### company commands

| Binding / behavior | Today (company) | Modern replacement | Notes |
|---|---|---|---|
| auto popup | `global-company-mode` | `global-corfu-mode` | |
| `C-<tab>` | `company-complete-common` | `completion-at-point` | insert common prefix / trigger |
| docs popup | company doc buffer | `corfu-popupinfo-mode` | signatures + docstrings |
| slow-tramp guard | `(company-mode -1)` on remote shell | `(corfu-mode -1)` on remote shell | same hook, same intent |
| backends | company backends | `cape-*` in `completion-at-point-functions` | dabbrev, file, keyword |
| LSP completion | `company-capf` | native `completion-at-point` | eglot feeds corfu directly |

---

## 3. Workarounds that disappear

1. **`helm-ag` read-only advice** (`init.el:135-140`) — added because Emacs 30
   made text-property functions honor `buffer-read-only`. Consult doesn't have
   this bug; delete the advice.
2. **`magit-merge` CRM override** (`init.el:409-413`) — added *because Helm can't
   do `completing-read-multiple`*. Vertico supports it natively, so drop the
   override and get real octopus-merge branch selection back.

---

## 4. What stays untouched

Nothing else references Helm or company internals. `magit`, `eglot`,
tree-sitter, `kubel`, `direnv`, `vterm`, the shell setup, `multiple-cursors`,
`flash`, `expreg`, and all helpers are unaffected. This is a self-contained swap
of a handful of config blocks.

---

## 5. Soft gap (honest caveat)

`helm-find-files` has a distinctive live navigation feel (type to filter, TAB to
descend, act on the fly). `find-file` + `vertico-directory` gets ~90% —
`RET`/`TAB` enter a dir, `DEL` goes up a component, `M-DEL` clears — but it's
*navigation-first*, not *action-first*. Most people prefer it after a day; a few
miss Helm's version. For recursive "find file anywhere in project" we actually
gain `consult-fd` / `project-find-file`, which are faster.

Everything else (including in-buffer completion) is a net upgrade or a wash.

---

## 6. Migration plan (phased, low-risk)

Each phase is independently committable and testable. Phases 1–2 give the full
minibuffer experience before anything Helm is removed.

**Phase 0 — Safety net.** Work on a `modernize-completion` branch so rollback is
one command. Repo is a git repo on `master`.

**Phase 1 — Stand up the vertico stack alongside Helm.** Add vertico + orderless
+ marginalia + savehist and enable them *without* removing Helm. Helm bindings
still work; generic `completing-read` prompts now use vertico. Live with it.

**Phase 2 — Add consult + embark + wgrep, rebind commands.** Point `M-i`,
`C-c g/G`, `C-x b`, `M-y`, `C-x C-r`, `C-x r b`, `M-r` at consult equivalents.
Rewrite `project/ag` as a one-line `consult-ripgrep` wrapper. Wire
`embark-export` → `wgrep` for the edit workflow. Bind `M-r` to `consult-history`
in `shell-mode-map` and **retire the vendored `pkgs/helm-comint/`** (drop the
`load-path` add + `(load "helm-comint.el")` at `init.el:433-434`).

**Phase 3 — Project & git-file browsing.** Switch `C-c f` / `C-x r p` to
project.el (or `consult-ls-git` for the git-tracked-file feel of helm-ls-git).

**Phase 4 — Remove Helm.** Delete the Helm blocks and the two workarounds, drop
`helm*` from `package-selected-packages`, confirm nothing references `helm-`,
byte-compile clean.

**Phase 5 — company → corfu + cape.** Replace `global-company-mode` with
`global-corfu-mode`; rebind `C-<tab>` to `completion-at-point`; enable
`corfu-popupinfo-mode`; add `cape-dabbrev` / `cape-file` / `cape-keyword` to
`completion-at-point-functions`; port the slow-tramp guard to `corfu-mode -1`.
Drop `company` from `package-selected-packages`. (Optional: `kind-icon` for
icons, `corfu-terminal` if Emacs is ever run in `-nw`.)

**Phase 6 — Polish (optional).** `savehist` tuning, `vertico-directory` keys,
orderless dispatchers (e.g. `!` to exclude), `marginalia` cycling.

---

## 7. Optional further modernization (orthogonal — noticed in config)

Not part of this migration, but several commented-out experiments exist:

- **gptel / aidermacs / copilot** — all commented out; pick one and wire it up.
- **eat vs vterm** — both configured; worth consolidating.
- **vundo** — commented `C-/` binding; trivial to enable.

---

## 8. Decisions needed before starting

- Start by **executing Phase 1** (add stack alongside Helm, nothing removed), or
  review a **full concrete diff** of the new config first?
- Keep migration **Helm-only for now**, or run the **corfu swap (Phase 5)** in
  the same pass?
- For `C-c f`: plain **`project-find-file`** (zero deps) or **`consult-ls-git`**
  (closest to helm-ls-git)?
