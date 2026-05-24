"""
Generate a v2 information-architecture document (task, story, sprint,
version, or component) from a Mustache template.

The script renders one of the templates under =library/templates/= and
writes the result, generating a fresh UUID for the document's =:ID:=
property.

Behaviour at a glance:

- Auto-detects parent info (=:ID:= and =#+title:=) by reading the
  parent file under =<parent-dir>/=. So you rarely need to type a UUID
  by hand.
- When run from a terminal, prompts for any required field that wasn't
  supplied on the command line. When run non-interactively, missing
  fields cause an error.
- Components live directly under their parent dir as =<slug>.org=;
  everything else gets its own =<slug>/<type>.org=.

See =doc/meta/document_types.org= for the contract every generated
document is expected to follow.
"""

import argparse
import sys
import uuid
from datetime import date
from pathlib import Path

import pystache

TEMPLATE_DIR = Path(__file__).resolve().parent.parent / "library" / "templates"

TYPE_TO_TEMPLATE = {
    "task": "v2_doc_task.org.mustache",
    "story": "v2_doc_story.org.mustache",
    "sprint": "v2_doc_sprint.org.mustache",
    "version": "v2_doc_version.org.mustache",
    "component": "v2_doc_component.org.mustache",
    "recipe": "v2_doc_recipe.org.mustache",
    "knowledge": "v2_doc_knowledge.org.mustache",
    "skill": "v2_doc_skill.org.mustache",
    "product_identity": "v2_doc_product_identity.org.mustache",
    "capture": "v2_doc_capture.org.mustache",
    "memory": "v2_doc_memory.org.mustache",
    "release_notes": "v2_doc_release_notes.org.mustache",
    "investigation": "v2_doc_investigation.org.mustache",
}

DEFAULT_INITIAL_STATE = {
    "task": "BACKLOG",
    "story": "BACKLOG",
    "sprint": "STARTED",
    "version": "STARTED",
    "component": "",
    "recipe": "",
    "knowledge": "",
    "skill": "",
    "product_identity": "",
    "capture": "",
    "release_notes": "",
    "memory": "",
    "investigation": "",
}

# Composition: each type's direct parent type.
PARENT_OF_TYPE = {
    "task": "story",
    "story": "sprint",
    "sprint": "version",
    # version, component, recipe, knowledge, skill, product_identity,
    # investigation have no composition parent.
}

# Types that don't take a parent (and aren't stateful).
PARENTLESS_TYPES = {
    "version", "component", "recipe", "knowledge", "skill", "product_identity",
    "capture", "memory", "release_notes", "investigation",
}


def build_filetags(tags_input, ancestor_slugs):
    """
    Combine user-supplied tags with ancestor-slug tags in org-mode form.

    `ancestor_slugs` is a list of slugs for the document's ancestors in
    the composition tree (immediate parent first). Stories carry their
    sprint + version slugs; tasks carry their story + sprint + version
    slugs; sprints carry their version slug. This lets a single grep
    find every task in a sprint (or version) without two-step lookups.

    Tag input accepts either commas or colons or already-formatted
    `:a:b:c:`. Empties and duplicates are dropped.
    """
    tags = []
    if tags_input:
        # Strip leading/trailing colons so ":a:b:" is accepted unchanged.
        stripped = tags_input.strip().strip(":")
        # Split on comma first; if no comma was present, fall through to colon.
        parts = stripped.split(",") if "," in stripped else stripped.split(":")
        for raw in parts:
            t = raw.strip()
            if t and t not in tags:
                tags.append(t)
    for slug in ancestor_slugs:
        if slug and slug not in tags:
            tags.append(slug)
    return ":" + ":".join(tags) + ":" if tags else ""


def derive_ancestor_slugs(doc_type, parent_dir):
    """
    Walk the composition tree upward from `parent_dir` and return the
    ancestor slugs as a list (immediate parent first).

    Tasks: [story, sprint, version]
    Stories: [sprint, version]
    Sprints: [version]
    Anything else: []

    Relies on the path convention versions/<version>/<sprint>/<story>/.
    """
    pd = Path(parent_dir)
    if doc_type == "task":
        return [pd.name, pd.parent.name, pd.parent.parent.name]
    if doc_type == "story":
        return [pd.name, pd.parent.name]
    if doc_type == "sprint":
        return [pd.name]
    return []


def read_parent_info(parent_file):
    """
    Read :ID: and #+title: from an existing parent document.

    Returns a dict with keys "id" and/or "title" (only those that were
    found). Returns an empty dict if the file doesn't exist.
    """
    info = {}
    if not parent_file.exists():
        return info
    for line in parent_file.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if stripped.startswith(":ID:") and "id" not in info:
            info["id"] = stripped[len(":ID:"):].strip()
        elif stripped.lower().startswith("#+title:") and "title" not in info:
            title = stripped[len("#+title:"):].strip()
            # Parent titles in tasks/stories carry a "Task: " / "Story: "
            # prefix; strip it so links read naturally in child docs.
            for prefix in ("Task:", "Story:"):
                if title.lower().startswith(prefix.lower()):
                    title = title[len(prefix):].strip()
                    break
            info["title"] = title
        if "id" in info and "title" in info:
            break
    return info


def is_interactive():
    return sys.stdin.isatty()


def prompt(label, default=None, choices=None):
    """Prompt for a value. Empty input returns the default. Re-prompts on invalid choice."""
    if choices:
        choice_text = "/".join(choices)
        suffix = f" [{default}]" if default else ""
        prompt_text = f"{label} ({choice_text}){suffix}: "
    else:
        suffix = f" [{default}]" if default else ""
        prompt_text = f"{label}{suffix}: "
    while True:
        try:
            answer = input(prompt_text).strip()
        except EOFError:
            sys.exit("\nerror: input closed before all required fields were supplied.")
        if not answer:
            answer = default or ""
        if choices and answer not in choices:
            print(f"  please pick one of {choice_text}")
            continue
        return answer


def fill_required(field, value, *, prompt_label, choices=None):
    """Return value if set; otherwise prompt (TTY) or error (non-TTY)."""
    if value:
        return value
    if is_interactive():
        return prompt(prompt_label, choices=choices)
    sys.exit(f"error: --{field} is required (and stdin is not interactive).")


def fill_optional(field, value, *, prompt_label, default=""):
    """Return value if set; otherwise prompt with default (TTY) or use default (non-TTY)."""
    if value:
        return value
    if is_interactive():
        return prompt(prompt_label, default=default or None)
    return default


def parse_args(argv=None):
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("--type", default="", choices=[""] + list(TYPE_TO_TEMPLATE))
    parser.add_argument("--slug", default="",
                        help="snake_case slug used for the folder/file name.")
    parser.add_argument("--parent-dir", default="",
                        help="Folder under which the new document is created.")
    parser.add_argument("--title", default="")
    parser.add_argument("--description", default="")
    parser.add_argument("--tags", default="",
                        help="Comma-separated content tags.")
    parser.add_argument("--owner", default="",
                        help="Task owner (handle). Default: marco.")
    parser.add_argument("--parent-id", default="",
                        help="UUID of the parent document. Auto-detected from <parent-dir>/ if omitted.")
    parser.add_argument("--parent-slug", default="",
                        help="Slug of the parent. Defaults to basename of <parent-dir>.")
    parser.add_argument("--parent-title", default="",
                        help="Title of the parent. Auto-detected from <parent-dir>/ if omitted.")
    parser.add_argument("--predecessor-id", default="",
                        help="Story only. UUID of a predecessor story (cross-sprint continuation).")
    parser.add_argument("--predecessor-title", default="",
                        help="Story only. Title of the predecessor for the inline link.")
    parser.add_argument("--state", default="",
                        help="Initial TODO state. Defaults per type.")
    parser.add_argument("--id", default="",
                        help="Use this UUID for :ID: instead of generating a fresh one. "
                             "Pass an existing document's UUID to preserve org-roam links "
                             "when migrating a file to v2 format.")
    parser.add_argument("--memory-subtype", default="feedback",
                        choices=["feedback", "user", "project", "reference"],
                        help="For --type memory: subtype of memory being stored. "
                             "Default feedback. Ignored for other types.")
    parser.add_argument("--force", action="store_true",
                        help="Overwrite the output file if it already exists.")
    return parser.parse_args(argv)


def main(argv=None):
    args = parse_args(argv)

    # Type and slug and parent-dir are required for anything to happen.
    args.type = fill_required("type", args.type,
                              prompt_label="Type",
                              choices=list(TYPE_TO_TEMPLATE))
    args.slug = fill_required("slug", args.slug, prompt_label="Slug (snake_case)")
    args.parent_dir = fill_required("parent-dir", args.parent_dir,
                                    prompt_label="Parent directory")
    parent_dir = Path(args.parent_dir)

    # Auto-detect parent info from the parent document if we can.
    if args.type not in PARENTLESS_TYPES:
        parent_type = PARENT_OF_TYPE[args.type]
        parent_file = parent_dir / f"{parent_type}.org"
        parent_info = read_parent_info(parent_file)
        if not args.parent_id and "id" in parent_info:
            args.parent_id = parent_info["id"]
        if not args.parent_title and "title" in parent_info:
            args.parent_title = parent_info["title"]
        if not args.parent_slug:
            args.parent_slug = parent_dir.name

        # Anything still missing → prompt or error.
        args.parent_id = fill_required("parent-id", args.parent_id,
                                       prompt_label=f"Parent {parent_type} ID")
        args.parent_title = fill_required("parent-title", args.parent_title,
                                          prompt_label=f"Parent {parent_type} title")

    # Required content fields.
    args.title = fill_required("title", args.title, prompt_label="Title")
    args.description = fill_required("description", args.description,
                                     prompt_label="Description (one-liner)")

    # Tasks and stories carry a "Task: " / "Story: " prefix in their
    # #+title so v1 and v2 docs are easy to tell apart at a glance.
    # Only add the prefix when not already present.
    if args.type == "task" and not args.title.lower().startswith("task:"):
        args.title = "Task: " + args.title
    elif args.type == "story" and not args.title.lower().startswith("story:"):
        args.title = "Story: " + args.title
    elif args.type == "investigation" and not args.title.lower().startswith("investigation:"):
        args.title = "Investigation: " + args.title

    # Optional fields.
    args.tags = fill_optional("tags", args.tags,
                              prompt_label="Tags (comma-separated, optional)")
    if args.type == "task":
        args.owner = fill_optional("owner", args.owner,
                                   prompt_label="Owner",
                                   default="marco")

    # Story-only fields are not prompted by default; they're set explicitly.
    if args.predecessor_id and args.type != "story":
        sys.exit("error: --predecessor-id is only valid for stories.")
    if args.predecessor_id and not args.predecessor_title:
        args.predecessor_title = fill_required(
            "predecessor-title", "",
            prompt_label="Predecessor title")

    state = args.state or DEFAULT_INITIAL_STATE.get(args.type, "")

    # Compose ancestor tags: walk the composition tree upward from the
    # parent dir. An explicit --parent-slug (if it doesn't coincide with
    # the derived immediate parent) is prepended so the explicit value
    # always lands as a tag.
    ancestor_slugs = derive_ancestor_slugs(args.type, parent_dir)
    if args.parent_slug and args.parent_slug not in ancestor_slugs:
        ancestor_slugs.insert(0, args.parent_slug)
    filetags = build_filetags(args.tags, ancestor_slugs)
    today = date.today().isoformat()
    new_id = (args.id if args.id else str(uuid.uuid4())).upper()

    # Captures derive their bucket (near | far) from the parent-dir name.
    bucket = parent_dir.name if args.type == "capture" else ""

    # Memories carry a subtype (feedback / user / project / reference) that
    # both lands as a filetag and appears in #+memory_subtype. The literal
    # "memory" tag is also injected so :memory: is greppable across the set.
    memory_subtype = args.memory_subtype if args.type == "memory" else ""
    if memory_subtype:
        injected = f"memory,{memory_subtype}"
        args.tags = f"{args.tags},{injected}" if args.tags else injected
        filetags = build_filetags(args.tags, ancestor_slugs)

    variables = {
        "id": new_id,
        "slug": args.slug,
        "title": args.title,
        "description": args.description,
        "filetags": filetags,
        "owner": args.owner or "marco",
        "date": today,
        "state": state,
        "parent_id": (args.parent_id or "").upper(),
        "parent_title": args.parent_title,
        "predecessor_id": (args.predecessor_id or "").upper(),
        "predecessor_title": args.predecessor_title or "",
        "bucket": bucket,
        "memory_subtype": memory_subtype,
    }

    template_path = TEMPLATE_DIR / TYPE_TO_TEMPLATE[args.type]
    template_text = template_path.read_text(encoding="utf-8")
    renderer = pystache.Renderer(escape=lambda value: value)
    rendered = renderer.render(template_text, variables)

    # Layouts:
    # - component: <parent-dir>/<slug>.org        (existing modeling convention)
    # - recipe:    <parent-dir>/<slug>.org        (slug typically how_do_i_*)
    # - knowledge: <parent-dir>/<slug>.org
    # - task:      <parent-dir>/task_<slug>.org   (prefix groups tasks under
    #              "t" so they sort below story.org and stand apart from any
    #              future siblings in the story folder)
    # - skill:     <parent-dir>/<slug>/SKILL.org  (Claude Code skill folder)
    # - story / sprint / version: <parent-dir>/<slug>/<type>.org
    #   (these are composition nodes; they hold children)
    if args.type == "task":
        # Don't double-prefix if the caller already passed task_<slug>.
        leaf = args.slug if args.slug.startswith("task_") else f"task_{args.slug}"
        out_dir = parent_dir
        out_file = out_dir / f"{leaf}.org"
    elif args.type in ("component", "recipe", "knowledge", "product_identity",
                       "capture", "memory", "investigation"):
        # Captures live at agile/product_backlog/<bucket>/<slug>.org. The
        # caller passes --parent-dir as that bucket directory; we validate
        # the bucket name only loosely (audit can tighten later).
        out_dir = parent_dir
        out_file = out_dir / f"{args.slug}.org"
    elif args.type == "skill":
        out_dir = parent_dir / args.slug
        out_file = out_dir / "SKILL.org"
    else:
        out_dir = parent_dir / args.slug
        out_file = out_dir / f"{args.type}.org"

    if out_file.exists() and not args.force:
        sys.exit(f"error: refusing to overwrite {out_file} (pass --force).")

    out_dir.mkdir(parents=True, exist_ok=True)
    out_file.write_text(rendered, encoding="utf-8")
    print(out_file)
    return 0


if __name__ == "__main__":
    sys.exit(main())
