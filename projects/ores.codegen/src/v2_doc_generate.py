"""
Generate a v2 information-architecture document (task, story, sprint,
or version) from a Mustache template.

The script renders one of the templates under =library/templates/= and
writes the result to =<parent-dir>/<slug>/<type>.org=, generating a
fresh UUID for the document's =:ID:= property.

See =doc/v2/meta/document_types.org= for the contract every generated
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
}

DEFAULT_INITIAL_STATE = {
    "task": "BACKLOG",
    "story": "BACKLOG",
    "sprint": "STARTED",
    "version": "STARTED",
}


def build_filetags(tags_csv, parent_slug):
    """Combine user-supplied tags with the parent-slug tag in org-mode form."""
    tags = []
    if tags_csv:
        tags.extend(t.strip() for t in tags_csv.split(",") if t.strip())
    if parent_slug and parent_slug not in tags:
        tags.append(parent_slug)
    return ":" + ":".join(tags) + ":" if tags else ""


def parse_args(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--type", required=True, choices=TYPE_TO_TEMPLATE)
    parser.add_argument("--slug", required=True,
                        help="snake_case slug — used for the folder name.")
    parser.add_argument("--parent-dir", required=True, type=Path,
                        help="Folder under which <slug>/<type>.org is created.")
    parser.add_argument("--title", required=True)
    parser.add_argument("--description", required=True)
    parser.add_argument("--tags", default="",
                        help="Comma-separated content tags.")
    parser.add_argument("--owner", default="marco",
                        help="Task owner (handle). Default: marco.")
    parser.add_argument("--parent-id", default="",
                        help="UUID of the parent document. Required for non-version types.")
    parser.add_argument("--parent-slug", default="",
                        help="Slug of the parent (added as a filetag for the parent-tag invariant).")
    parser.add_argument("--parent-title", default="",
                        help="Human-readable title of the parent. Required for non-version types.")
    parser.add_argument("--predecessor-id", default="",
                        help="Story only. UUID of a predecessor story (cross-sprint continuation).")
    parser.add_argument("--predecessor-title", default="",
                        help="Story only. Title of the predecessor for the inline link.")
    parser.add_argument("--state", default="",
                        help="Initial TODO state. Defaults: task/story=BACKLOG, sprint/version=STARTED.")
    parser.add_argument("--force", action="store_true",
                        help="Overwrite the output file if it already exists.")
    return parser.parse_args(argv)


def main(argv=None):
    args = parse_args(argv)

    if args.type != "version":
        if not args.parent_id:
            sys.exit("error: --parent-id is required for non-version types.")
        if not args.parent_title:
            sys.exit("error: --parent-title is required for non-version types.")

    if args.predecessor_id and args.type != "story":
        sys.exit("error: --predecessor-id is only valid for stories.")

    state = args.state or DEFAULT_INITIAL_STATE[args.type]
    filetags = build_filetags(args.tags, args.parent_slug)
    today = date.today().isoformat()
    new_id = str(uuid.uuid4())

    variables = {
        "id": new_id,
        "title": args.title,
        "description": args.description,
        "filetags": filetags,
        "owner": args.owner,
        "date": today,
        "state": state,
        "parent_id": args.parent_id,
        "parent_title": args.parent_title,
        "predecessor_id": args.predecessor_id or "",
        "predecessor_title": args.predecessor_title or "",
    }

    template_path = TEMPLATE_DIR / TYPE_TO_TEMPLATE[args.type]
    template_text = template_path.read_text(encoding="utf-8")
    renderer = pystache.Renderer(escape=lambda value: value)
    rendered = renderer.render(template_text, variables)

    out_dir = args.parent_dir / args.slug
    out_file = out_dir / f"{args.type}.org"
    if out_file.exists() and not args.force:
        sys.exit(f"error: refusing to overwrite {out_file} (pass --force).")

    out_dir.mkdir(parents=True, exist_ok=True)
    out_file.write_text(rendered, encoding="utf-8")
    print(out_file)
    return 0


if __name__ == "__main__":
    sys.exit(main())
