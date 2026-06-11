# ORE Studio — Claude Code Instructions

Run `./compass.sh bearings` to orient yourself before acting.

**Memory rule:** All project memories live in `doc/llm/memory/` and must
be created with `compass add memory`. Never write to the Claude Code
harness memory system (`~/.claude/projects/*/memory/`). The harness
auto-memory is disabled via `autoMemoryEnabled: false` in
`.claude/settings.json`.
