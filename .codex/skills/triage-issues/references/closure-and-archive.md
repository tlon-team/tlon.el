# GitHub closure and Org archiving

Read only when the current request authorizes one of these operations. Neither
triage nor a local fix is permission to publish or remove a task from active notes.

## GitHub identity and credentials

Read the checkout's `CLAUDE.md` and the governing secrets policy before handling
credentials. The project documents `pass env/tlon-ai-github-token`; use the
configured non-printing secret workflow, not a presumed ambient
`TLON_AI_GITHUB_TOKEN`. Preserve an existing canonical Tlon entry under `tlon/`
rather than duplicating it under `env/`. Do not migrate or rotate credentials
as part of issue triage.

Use the `tlon-ai` account for AI-authored comments and closure. Explicitly supply
the selected credential as `GH_TOKEN` for every operation in that sequence.
Check the successful identity response and require its login to equal
`tlon-ai` before writing. Missing credentials, an API error or a different
login means no write; never fall back to the personal account. Keep tokens out
of output, files, debug traces, comments and the kill ring.

For github.com, `GH_TOKEN` precedes `GITHUB_TOKEN`; both precede stored
credentials. Token prefix is not proof of identity or authorization. GitHub's
comment/create and issue/update endpoints support fine-grained tokens with
appropriate repository permissions; organization policy or approval can impose
additional constraints. Do not prescribe a classic PAT or broad `repo` scope
without evidence that this workflow requires it.

## Checked publication and closure

1. Confirm exactly which comments, closure and publication actions the user
   authorized. A local fix does not imply permission to push or open a PR.
   Before describing a fix as delivered, verify that the relevant commit is
   available in the intended remote repository and satisfies the issue's
   integration/acceptance requirement. A local SHA or remote feature branch
   alone is not evidence of an integrated fix. If delivery is pending, report
   that state and do not close as fixed. A separately authorized non-fix closure
   must use its actual reason, not invented fix evidence.
2. Refresh the exact issue and relevant discussion; ensure it is the intended
   issue rather than a PR. If already closed or already commented on, reconcile
   the actual state before deciding whether another write is needed.
3. After the bot identity check, post only the authorized comment, using a
   safely supplied body rather than interpolating issue text into shell code.
   Include the accessible fix reference and relevant acceptance evidence.
   Use the user's voice skill only if explicitly drafting as the user; ordinary
   AI-authored `tlon-ai` comments must not impersonate the user.
4. Require successful command/API results and a valid comment identifier.
   Read that exact comment back and check repository/issue association, author
   `tlon-ai`, and expected content. Only then perform an authorized closure.
   Check the close result and read the exact issue back to confirm closed state.
   Printing login, author or state without comparing the expected values is
   not a gate. Stop later mutations after any failed or mismatched check.
5. A timeout after submission leaves the result uncertain. Inspect persisted
   comments and issue state before retrying; do not duplicate a comment or
   repeat closure because its acknowledgement was lost. Keep the record of a
   successful comment if closure fails; do not claim the whole sequence succeeded.
   Never post a test comment or change account permissions just to test access.

## Org archive

The default note is
`/Users/pablostafforini/My Drive/tlon-notes/tlon.el.org`. Preserve an explicitly
selected note. Archiving requires its own scope: GitHub closure alone is not
permission to archive. An explicit local-only archive need not close GitHub.

1. Follow `org-note-conventions`. Check both source and actual archive
   destination, their visiting buffers, current disk state and archive/TODO
   configuration before mutation. Work with the authoritative current buffer;
   do not overwrite unsaved edits with a fresh batch read. Use the configured
   Emacs context, or an isolated batch only when all necessary configuration
   and absence of competing/unsaved writers are established. Do not restart
   Emacs or evaluate untrusted note-local code to obtain that context.
2. Identify the eligible heading structurally by the exact repository and issue
   identity: a validated topic ID/link, or a verified repository heading label
   plus exact issue number. The note's repository context may disambiguate a
   legacy heading only when established. Ignore body-text mentions, prefix
   matches such as #12 versus #123, and headings already in the archive.
   Gather all eligible matches before editing. Zero matches means report not
   found; more than one means leave the file unchanged and resolve ambiguity.
   Do not use an unanchored first `#NUMBER` text search.
3. Preserve the selected subtree and surrounding content. Mark the intended
   TODO state and use the configured Org archive operation for that subtree.
   `org-archive-subtree-default` dispatches to a configurable command; it does
   not itself guarantee a CLOSED timestamp or an in-file Archive heading.
   Inspect the resolved destination and required metadata rather than assuming
   the fresh batch defaults match the user's configuration.
4. Avoid `tlon-close-issue-and-todo` for local-only archiving: it also closes
   GitHub. Archive operations must not silently invoke a coupled remote action.
5. Save only the authorized source/destination changes, then read back both.
   Verify the target left the active section and appears exactly once at the
   intended archive destination with its content/IDs preserved; unrelated
   headings must remain unchanged. If already archived, verify that state
   without appending a duplicate. On partial failure, report the observed state
   and reconcile before retrying; do not overwrite an intervening edit.

## Primary references

- [GitHub CLI credential precedence](https://cli.github.com/manual/gh_help_environment)
- [Create an issue comment](https://docs.github.com/en/rest/issues/comments#create-an-issue-comment)
- [Update an issue](https://docs.github.com/en/rest/issues/issues#update-an-issue)

For Org behavior, inspect the installed command documentation and effective
configuration; archive destination, completion logging and saving are configurable.
