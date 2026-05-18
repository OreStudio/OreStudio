# v2 document generator

`generate_v2_doc.sh` creates a new v2 information-architecture document
(task, story, sprint, or version) from a Mustache template, producing a
file that already follows the contract in
`doc/v2/meta/document_types.org`.

## What it generates

| Type | Output path |
|------|-------------|
| task | `<parent-dir>/<slug>/task.org` |
| story | `<parent-dir>/<slug>/story.org` |
| sprint | `<parent-dir>/<slug>/sprint.org` |
| version | `<parent-dir>/<slug>/version.org` |
| component | `<parent-dir>/<slug>.org` (no subfolder — matches the existing `projects/<comp>/modeling/<comp>.org` convention) |

Each output has a fresh UUID in `:ID:`, today's date in `#+created`
and `#+updated`, the standard frontmatter for its type, an initial
`* Status` headline at the type's default TODO state, and skeleton
sections.

## Usage

```sh
projects/ores.codegen/generate_v2_doc.sh \
  --type <task|story|sprint|version|component> \
  --slug <snake_case_slug> \
  --parent-dir <path-where-slug-folder-will-be-created> \
  --title "<human-readable title>" \
  --description "<one-liner ≤ 120 chars>" \
  [--tags "tag1,tag2,..."] \
  [--owner <handle>]                          # tasks; default: marco
  [--parent-id <uuid>]                        # required for non-{version,component}
  [--parent-slug <slug>]                      # added as filetag (parent-tag invariant)
  [--parent-title "<title>"]                  # required for non-{version,component}
  [--predecessor-id <uuid>]                   # story only, cross-sprint continuation
  [--predecessor-title "<title>"]
  [--state <BACKLOG|DISCOVERED|STARTED>]
  [--force]
```

The script prints the path of the file it wrote.

## Example — add a component model doc

```sh
projects/ores.codegen/generate_v2_doc.sh \
  --type component --slug ores.example \
  --parent-dir projects/ores.example/modeling \
  --title "ores.example" \
  --description "One-line summary of what the component does." \
  --tags "example,scaffolding"
```

Components have no parent in the composition tree, so `--parent-id`,
`--parent-slug`, and `--parent-title` are optional and ignored.

## Example — start a new sprint

```sh
projects/ores.codegen/generate_v2_doc.sh \
  --type sprint --slug sprint_17 \
  --parent-dir doc/v2/versions/v0 \
  --title "Sprint 17" \
  --description "Sprint 17 — describe its mission in one sentence." \
  --tags "v0" \
  --parent-id <uuid-of-version-v0> \
  --parent-slug v0 \
  --parent-title "Version 0"
```

## Example — add a story to the current sprint

```sh
projects/ores.codegen/generate_v2_doc.sh \
  --type story --slug improve_audit_signals \
  --parent-dir doc/v2/versions/v0/sprint_17 \
  --title "Improve audit signals" \
  --description "Surface stale tasks, orphan plans, and broken links." \
  --tags "audit,scripts" \
  --parent-id <uuid-of-sprint-17> \
  --parent-slug sprint_17 \
  --parent-title "Sprint 17"
```

## Example — continue a cross-sprint story

```sh
projects/ores.codegen/generate_v2_doc.sh \
  --type story --slug currencies_temporal_continued \
  --parent-dir doc/v2/versions/v0/sprint_17 \
  --title "Currencies temporal (continued)" \
  --description "Pick up where sprint 02 left off." \
  --tags "currencies,temporal,reference_data" \
  --parent-id <uuid-of-sprint-17> \
  --parent-slug sprint_17 \
  --parent-title "Sprint 17" \
  --predecessor-id <uuid-of-prior-story> \
  --predecessor-title "Currencies temporal and export"
```

After the script writes the successor, update the *predecessor* story
to point forward — add `#+successor: <new-uuid>` to its frontmatter
and a `Continued in: [[id:...][...]]` note in `* Decisions`. There is
no automated way to do this yet; do it by hand or extend the script.

## Templates

The Mustache sources live under
`projects/ores.codegen/library/templates/`:

- `v2_doc_task.org.mustache`
- `v2_doc_story.org.mustache`
- `v2_doc_sprint.org.mustache`
- `v2_doc_version.org.mustache`

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

(System pystache also works if you invoke `src/v2_doc_generate.py`
directly with `python3`.)
