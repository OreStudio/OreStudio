# ORE Studio — Claude Code Instructions

Run `./compass.sh bearings` to orient yourself before acting.

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
