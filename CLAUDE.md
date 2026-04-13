# tlon.el conventions

## Smart quotes

- Unicode curly/smart quotes (U+201C, U+201D) used AS string delimiters → replace with ASCII `"`.
- Unicode curly/smart quotes INSIDE already-delimited strings → escape as `\"`.
- Unicode curly/smart quotes inside docstrings → same; unescaped ASCII `"` terminates the docstring.
- Never do blanket find-and-replace without distinguishing these cases.

## GitHub bot account

When posting comments, replies, or any public-facing GitHub activity on behalf of AI/Claude on tlon repos, use the `tlon-ai` bot account (token: `TLON_AI_GITHUB_TOKEN` in `~/.zshenv-secrets`), not the user's personal `benthamite` account.
