# Document generator

`compass add <type>` creates a new information-architecture document
(task, story, sprint, or version) from a Mustache template, producing a
file that already follows the contract in
`doc/meta/document_types.org`.

## What it generates

| Type | Output path |
|------|-------------|
| task | `<parent-dir>/task_<slug>.org` (flat file under the story folder; `task_` prefix sorts tasks together and makes their type obvious in `ls`). Pass `--slug add_thing` and the generator prepends `task_` — passing `task_add_thing` works too and isn't double-prefixed. |
| story | `<parent-dir>/<slug>/story.org` |
| sprint | `<parent-dir>/<slug>/sprint.org` |
| version | `<parent-dir>/<slug>/version.org` |
| component | `<parent-dir>/<slug>.org` (no subfolder — matches the existing `projects/<comp>/modeling/<comp>.org` convention) |
| recipe | `<parent-dir>/<slug>.org` (slug typically starts with `how_do_i_`) |
| knowledge | `<parent-dir>/<slug>.org` |
| skill | `<parent-dir>/<slug>/SKILL.org` (slug becomes both the folder name and the Claude Code skill `name:`) |
| investigation | `<parent-dir>/investigation_<slug>.org` (flat file under the story folder that commissioned it; `--parent-dir` is required — an investigation belongs to a story, not to a fixed directory). See `doc/meta/document_type_investigation.org`: it is a point-in-time record, and its durable conclusions are promoted to the page that owns the subject. |

Each output has a fresh UUID in `:ID:` (or a caller-supplied UUID via
`--id` — see below), today's date in `#+created` and `#+updated`, the
standard frontmatter for its type, an initial `* Status` headline at the
type's default TODO state, and skeleton sections.

## Usage

```sh
compass add <task|story|sprint|version|component|recipe|knowledge|skill> \
  [--slug <snake_case_slug>] \
  [--parent-dir <path-where-the-new-doc-is-created>] \
  [--title "<human-readable title>"] \
  [--description "<one-liner ≤ 120 chars>"] \
  [--tags "tag1,tag2,..."] \           # commas or colons (`:tag1:tag2:`) both accepted
  [--owner <handle>]                          # tasks; default: marco
  [--parent-id <uuid>]                        # auto-detected from <parent-dir>/<parent-type>.org
  [--parent-slug <slug>]                      # defaults to basename of <parent-dir>
  [--parent-title "<title>"]                  # auto-detected from <parent-dir>/<parent-type>.org
  [--predecessor-id <uuid>]                   # story only, cross-sprint continuation
  [--predecessor-title "<title>"]
  [--state <BACKLOG|DISCOVERED|STARTED>]
  [--id <uuid>]                               # preserve an existing UUID (migration only)
  [--force]
```

The generator prints the path of the file it wrote.

### Auto-detection of parent info

For task / story / sprint, the parent document's `:ID:` and `#+title:`
are read automatically from `<parent-dir>/<parent-type>.org`. So you
typically only need the type, `--slug`, `--parent-dir`, `--title`,
`--description`, `--tags`. The parent IDs and titles get filled in for
you.

For example, when creating a task with
`--parent-dir doc/agile/versions/v0/sprint_17/audit_tooling`, the generator
reads `.../audit_tooling/story.org`, picks up its `:ID:` and
`#+title:`, and uses `audit_tooling` as the parent slug (from the
folder basename).

### Preserving an existing UUID (--id)

When migrating a document that already has an `:ID:` to v2 format, pass
the existing UUID via `--id`. The generator uses it verbatim instead of
minting a fresh one. This keeps all org-roam backlinks intact.

```sh
compass add component \
  --slug component_overview \
  --parent-dir projects/ores.trading.core/modeling \
  --title "ores.trading" \
  --description "Trade booking and lifecycle management." \
  --tags "trading,component" \
  --id "E9A3F7B2-6C14-4D8E-A5B9-3F2D1C0E7A6B"
```

Omit `--id` for brand-new documents; the generator mints a fresh UUID.

### Interactive prompts

When run from a terminal, any missing required field is prompted for.
When run non-interactively (piped, hooks, CI), missing required fields
cause a clear error rather than hanging.

So the shortest interactive invocation is:

```sh
compass add
```

— and the generator walks you through type, slug, parent dir, title,
description, and tags.

## Example — add a recipe

```sh
compass add recipe \
  --slug how_do_i_clear_the_cache \
  --parent-dir doc/recipes/cmake \
  --title "How do I clear the cache?" \
  --description "Remove the CMake binary cache to force a clean re-configure." \
  --tags "cmake,build,recipe"
```

## Example — add a knowledge document

```sh
compass add knowledge \
  --slug build_system_decisions \
  --parent-dir doc/knowledge/architecture \
  --title "Build system decisions" \
  --description "Why we picked Ninja over Make as the default generator." \
  --tags "build,architecture,knowledge"
```

## Example — add a skill

```sh
compass add skill \
  --slug my-new-skill \
  --parent-dir doc/skills \
  --title "My New Skill" \
  --description "When and how to use the new skill." \
  --tags "skill"
```

The slug uses kebab-case for skills (matches existing convention like
`cmake-runner`). It becomes both the folder name and the `name:` field
in the Claude Code markdown frontmatter.

## Example — add a component model doc

```sh
compass add component \
  --slug ores.example \
  --parent-dir projects/ores.example/modeling \
  --title "ores.example" \
  --description "One-line summary of what the component does." \
  --tags "example,scaffolding"
```

Components have no parent in the composition tree, so `--parent-id`,
`--parent-slug`, and `--parent-title` are optional and ignored.

## Example — start a new sprint

```sh
compass add sprint \
  --slug sprint_17 \
  --parent-dir doc/agile/versions/v0 \
  --title "Sprint 17" \
  --description "Sprint 17 — describe its mission in one sentence." \
  --tags "v0" \
  --parent-id <uuid-of-version-v0> \
  --parent-slug v0 \
  --parent-title "Version 0"
```

## Example — add a story to the current sprint

```sh
compass add story \
  --slug improve_audit_signals \
  --parent-dir doc/agile/versions/v0/sprint_17 \
  --title "Improve audit signals" \
  --description "Surface stale tasks, orphan plans, and broken links." \
  --tags "audit,scripts" \
  --parent-id <uuid-of-sprint-17> \
  --parent-slug sprint_17 \
  --parent-title "Sprint 17"
```

## Example — continue a cross-sprint story

```sh
compass add story \
  --slug currencies_temporal_continued \
  --parent-dir doc/agile/versions/v0/sprint_17 \
  --title "Currencies temporal (continued)" \
  --description "Pick up where sprint 02 left off." \
  --tags "currencies,temporal,reference_data" \
  --parent-id <uuid-of-sprint-17> \
  --parent-slug sprint_17 \
  --parent-title "Sprint 17" \
  --predecessor-id <uuid-of-prior-story> \
  --predecessor-title "Currencies temporal and export"
```

After the generator writes the successor, update the *predecessor* story
to point forward — add `#+successor: <new-uuid>` to its frontmatter
and a `Continued in: [[id:...][...]]` note in `* Decisions`. There is
no automated way to do this yet; do it by hand or extend the generator.

## Templates

The Mustache sources live under
`projects/ores.codegen/library/templates/`:

- `doc_task.org.mustache`
- `doc_story.org.mustache`
- `doc_sprint.org.mustache`
- `doc_version.org.mustache`

Edit these to change the skeleton sections or default placeholders.
Variables available to all templates: `id`, `title`, `description`,
`filetags`, `date`, `state`, `parent_id`, `parent_title`. Tasks
additionally have `owner`. Stories additionally have `predecessor_id`
and `predecessor_title` (rendered conditionally).

## Setup

The wrapper expects the codegen virtualenv:

```sh
cd projects/ores.codegen
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```

(System pystache also works if you invoke `src/doc_generate.py`
directly with `python3`.)
