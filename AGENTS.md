# tlon.el conventions

## Smart quotes

- Unicode curly/smart quotes (U+201C, U+201D) used AS string delimiters → replace with ASCII `"`.
- Unicode curly/smart quotes INSIDE already-delimited strings → escape as `\"`.
- Unicode curly/smart quotes inside docstrings → same; unescaped ASCII `"` terminates the docstring.
- Never do blanket find-and-replace without distinguishing these cases.

## GitHub bot account

When posting comments, replies, or any public-facing GitHub activity on behalf of AI/Claude on tlon repos, use the `tlon-ai` bot account, not the user's personal `benthamite` account. Read the token from the `tlon-ai-github-token` item of the personal 1Password Automation vault:

```bash
GH_TOKEN="$(op-automations @personal read "op://Automation/tlon-ai-github-token/password")" gh <command> ...
```

Before posting, verify the token resolves to the right account:

```bash
GH_TOKEN="$(op-automations @personal read "op://Automation/tlon-ai-github-token/password")" gh api user --jq .login
# must print: tlon-ai
```

Never silently fall back to the default `gh` auth — if `op-automations` can't read the item or the token resolves to a different account, stop and tell me.
