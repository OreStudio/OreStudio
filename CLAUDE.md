# ORE Studio — Claude Code Instructions

Run `./compass.sh bearings` to orient yourself before acting.

**ASD-STE100 skill:** After bearings, invoke the `compass-doc-review-ste100`
skill via the Skill tool. It applies ASD-STE100 Simplified Technical
English rules to all user-facing prose agent output: one meaning per
word, active voice, simple tense, one instruction per sentence, short
sentences. Do not apply STE100 rewriting to structured output — JSON,
diffs, code, commit messages, or command output.

**Knowledge pages (Zettelkasten pages):** any page under `doc/knowledge`
follows the house style in `doc/knowledge/documentation/zettelkasten.org`,
sections *Creating a knowledge document* and *The house style*. Read them
before writing. In short: search first and link instead of repeating;
academic prose; every claim derived, cited or labelled unsupported, with
academic papers preferred and private notes never cited; a title that reads
like an academic paper title (a noun phrase, never a sentence); the file
name is the title in snake case. Draw a PlantUML diagram, rendered to PNG
and embedded with a caption, wherever a lifecycle, a component interaction
or a structure is described. Keep the data source out of general pages: no
"observed or simulated", no account of how values are generated; link to the
synthetic generation page that owns those details instead. Review with the
`compass-doc-review-academic` skill before committing.

**Code comments:** the `compass-code-review-comments` skill sets the rules for
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
When a command is chatty and prints no log path of its own — a tangle, a
regenerate — run it as a background job so the harness collects the
output. Redirecting it to a temp file to keep it out of the transcript is
still a redirect, and it throws away the output this rule exists to keep.

**Memory rule:** All project memories live in `doc/llm/memory/` and must
be created with `compass add memory`. Never write to the Claude Code
harness memory system (`~/.claude/projects/*/memory/`). The harness
auto-memory is disabled via `autoMemoryEnabled: false` in
`.claude/settings.json`.
