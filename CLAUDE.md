# ORE Studio — Claude Code Instructions

Run `./compass.sh bearings` to orient yourself before acting.

**ASD-STE100 skill:** After bearings, invoke the `doc-review-ste100`
skill via the Skill tool. It applies ASD-STE100 Simplified Technical
English rules to all user-facing prose agent output: one meaning per
word, active voice, simple tense, one instruction per sentence, short
sentences. Do not apply STE100 rewriting to structured output — JSON,
diffs, code, commit messages, or command output.

**Code comments:** the `code-review-comments` skill sets the rules for
every comment in code we create or edit: use comments sparingly, remove
commented-out code, never narrate edit history, never use end-of-line
comments, place comments above the code. Apply these rules as you
write; invoke the skill for a cleanup pass.

**Never pipe or redirect a `compass` command.** Run it bare — no `|`,
no `2>&1`, no `>`. Every `compass build`/`deploy`/`site` command that
produces meaningful output prints its own well-known log file path
(e.g. `📝 Build output: /tmp/<label>_<target>_build.log`). To watch
progress, `tail -f`/`tail -n` *that reported file* as a separate,
standalone command — never by piping the compass invocation itself.

**Memory rule:** All project memories live in `doc/llm/memory/` and must
be created with `compass add memory`. Never write to the Claude Code
harness memory system (`~/.claude/projects/*/memory/`). The harness
auto-memory is disabled via `autoMemoryEnabled: false` in
`.claude/settings.json`.
