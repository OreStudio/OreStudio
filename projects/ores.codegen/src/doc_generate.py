"""
Generate an information-architecture document (task, story, sprint,
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
from datetime import date, timedelta
from pathlib import Path

import pystache

TEMPLATE_DIR = Path(__file__).resolve().parent.parent / "library" / "templates"

TYPE_TO_TEMPLATE = {
    "task": "doc_task.org.mustache",
    "story": "doc_story.org.mustache",
    "sprint": "doc_sprint.org.mustache",
    "version": "doc_version.org.mustache",
    "component": "doc_component.org.mustache",
    "recipe": "doc_recipe.org.mustache",
    "knowledge": "doc_knowledge.org.mustache",
    "manual": "doc_manual.org.mustache",
    "skill": "doc_skill.org.mustache",
    "product_identity": "doc_product_identity.org.mustache",
    "capture": "doc_capture.org.mustache",
    "memory": "doc_memory.org.mustache",
    "release_notes": "doc_release_notes.org.mustache",
    "investigation": "doc_investigation.org.mustache",
    "runbook": "doc_runbook.org.mustache",
    "entity_org": "doc_entity_org.org.mustache",
    "field_group": "doc_field_group.org.mustache",
    "table": "doc_table.org.mustache",
    "junction": "doc_junction.org.mustache",
    "lookup_entity": "doc_lookup_entity.org.mustache",
    "service_registry": "doc_service_registry.org.mustache",
    "dataset_overview": "doc_dataset.org.mustache",
    "facet": "doc_facet.org.mustache",
    "facet_group": "doc_facet_group.org.mustache",
    "technical_space": "doc_technical_space.org.mustache",
    "archetype": "doc_archetype.org.mustache",
}

DEFAULT_INITIAL_STATE = {
    "task": "BACKLOG",
    "story": "BACKLOG",
    "sprint": "STARTED",
    "version": "STARTED",
    "component": "",
    "recipe": "",
    "knowledge": "",
    "manual": "",
    "skill": "",
    "product_identity": "",
    "capture": "",
    "release_notes": "",
    "memory": "",
    "investigation": "",
    "runbook": "",
    "entity_org": "",
    "field_group": "",
    "table": "",
    "junction": "",
    "lookup_entity": "",
    "service_registry": "",
    "dataset_overview": "",
    "facet": "",
    "facet_group": "",
    "technical_space": "",
    "archetype": "",
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
    "version", "component", "recipe", "knowledge", "manual", "skill", "product_identity",
    "capture", "memory", "release_notes", "investigation", "runbook",
    "entity_org", "field_group", "table", "junction", "lookup_entity",
    "service_registry", "dataset_overview",
    "facet", "facet_group", "technical_space", "archetype",
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
                             "when reformatting an existing document.")
    parser.add_argument("--memory-subtype", default="feedback",
                        choices=["feedback", "user", "project", "reference"],
                        help="For --type memory: subtype of memory being stored. "
                             "Default feedback. Ignored for other types.")
    parser.add_argument("--statement", default="",
                        help="For --type memory: the rule/fact/insight itself "
                             "(the opening body paragraph). Optional; the "
                             "template placeholder remains when omitted.")
    parser.add_argument("--why", default="",
                        help="For --type memory: the reason or incident behind "
                             "the rule. Optional.")
    parser.add_argument("--how-to-apply", dest="how_to_apply", default="",
                        help="For --type memory: when/where the memory should "
                             "kick in. Optional.")
    parser.add_argument("--facet-group", dest="facet_group", default="",
                        help="For --type facet: slug of the facet group this "
                             "facet belongs to (cmake, cpp, sql, doc, assets). "
                             "Lands in #+facet_group:, which the facet "
                             "inventory regenerator dispatches on.")
    parser.add_argument("--component", default="",
                        help="For --type entity_org/field_group/table/junction/"
                             "lookup_entity: short component name "
                             "(refdata, trading, ...). Drives the output path "
                             "(projects/ores.<component>/modeling/) and the "
                             "ores.<component>.<slug> title.")
    parser.add_argument("--entity-plural", dest="entity_plural", default="",
                        help="For --type entity_org/table/lookup_entity/junction: "
                             "snake_case plural noun (defaults to slug + 's' when "
                             "omitted).")
    parser.add_argument("--has-tenant-id", dest="has_tenant_id", default="true",
                        choices=["true", "false"],
                        help="For --type table/lookup_entity/junction: Default: true.")
    parser.add_argument("--coding-scheme", dest="coding_scheme", default="none",
                        choices=["none", "required", "nullable"],
                        help="For --type table: Default: none.")
    parser.add_argument("--dataset", default="",
                        help="For --type dataset_overview: dataset name "
                             "(e.g. slovaris). Drives the output path "
                             "(projects/ores.seeder/datasets/<name>/) and "
                             "the #+dataset_name: keyword.")
    parser.add_argument("--dataset-version", default="1.0",
                        help="For --type dataset_overview: version string. "
                             "Default: 1.0.")
    parser.add_argument("--dataset-type", default="crafted",
                        help="For --type dataset_overview: crafted or synthesised. "
                             "Default: crafted.")
    parser.add_argument("--source-methodology", default="",
                        help="For --type dataset_overview: brief description of "
                             "where the data came from.")
    parser.add_argument("--goal", default="",
                        help="Goal prose for task/story docs; defaults to "
                             "the fill-in placeholder.")
    parser.add_argument("--acceptance", action="append", default=[],
                        help="An acceptance bullet for task/story docs "
                             "(repeatable).")
    parser.add_argument("--intro", default="",
                        help="For --type recipe: lead paragraph pointing at "
                             "the component model / knowledge doc the recipe "
                             "relies on. Defaults to the fill-in placeholder.")
    parser.add_argument("--question", default="",
                        help="For --type recipe: the NLP question the recipe "
                             "answers (* Question body).")
    parser.add_argument("--answer", default="",
                        help="For --type recipe: the * Answer body, verbatim "
                             "org markup (may be multi-line, e.g. a "
                             "#+begin_src block).")
    parser.add_argument("--script", default="",
                        help="For --type recipe: the * Script body — pointer "
                             "to the script or wrapper that does the work.")
    parser.add_argument("--tested-by", default="",
                        help="For --type recipe: the * Tested by body — how "
                             "the recipe is exercised.")
    parser.add_argument("--see-also", action="append", default=[],
                        help="For --type recipe: a * See also bullet, "
                             "verbatim org markup (repeatable).")
    parser.add_argument("--brief", default="",
                        help="For --type component: one-line tagline read by codegen "
                             "from #+brief:. Also accepted for entity_org, field_group, "
                             "and junction to pre-fill the #+brief: frontmatter keyword. "
                             "Ignored for other types.")
    parser.add_argument("--force", action="store_true",
                        help="Overwrite the output file if it already exists.")
    return parser.parse_args(argv)


def main(argv=None):
    args = parse_args(argv)

    # Type is required before any type-specific defaulting.
    args.type = fill_required("type", args.type,
                              prompt_label="Type",
                              choices=list(TYPE_TO_TEMPLATE))

    # entity_org, field_group, table, junction, lookup_entity derive their
    # parent dir from the component.
    _COMPONENT_TYPES = ("entity_org", "field_group", "table", "junction", "lookup_entity")
    if args.type in _COMPONENT_TYPES:
        args.component = fill_required(
            "component", args.component,
            prompt_label="Component (refdata, trading, ...)")
        if not args.parent_dir:
            args.parent_dir = f"projects/ores.{args.component}/modeling"

    # service_registry lives in projects/modeling by default; slug is fixed
    # (the output path ignores it, matching dataset_overview's convention).
    if args.type == "service_registry":
        if not args.parent_dir:
            args.parent_dir = "projects/modeling"
        if not args.slug:
            args.slug = "service_registry"
        if not args.title:
            args.title = "Service registry"

    # facet and facet_group docs live with the templates they tangle.
    if args.type in ("facet", "facet_group") and not args.parent_dir:
        args.parent_dir = "projects/ores.codegen/library/templates"
    if args.type == "facet":
        args.facet_group = fill_required(
            "facet-group", args.facet_group,
            prompt_label="Facet group (cmake, cpp, sql, doc, assets)")

    # dataset_overview derives parent dir and slug defaults from the dataset name.
    if args.type == "dataset_overview":
        args.dataset = fill_required(
            "dataset", args.dataset,
            prompt_label="Dataset name (e.g. slovaris)")
        if not args.parent_dir:
            args.parent_dir = f"projects/ores.seeder/datasets/{args.dataset}"
        if not args.slug:
            args.slug = "dataset_overview"
        if not args.title:
            args.title = args.dataset

    # slug fill runs after type-specific defaults so dataset_overview can set it.
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

    # Required content fields. All component-scoped types derive title from
    # component + slug.
    if args.type in _COMPONENT_TYPES and not args.title:
        args.title = f"ores.{args.component}.{args.slug}"
    args.title = fill_required("title", args.title, prompt_label="Title")
    args.description = fill_required("description", args.description,
                                     prompt_label="Description (one-liner)")

    # Tasks and stories carry a "Task: " / "Story: " prefix in their
    # #+title so their type is obvious at a glance.
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

    # Captures derive their bucket (next | deferred) from the parent-dir name.
    bucket = parent_dir.name if args.type == "capture" else ""

    # Memories carry a subtype (feedback / user / project / reference) that
    # both lands as a filetag and appears in #+memory_subtype. The literal
    # "memory" tag is also injected so :memory: is greppable across the set.
    memory_subtype = args.memory_subtype if args.type == "memory" else ""
    if memory_subtype:
        injected = f"memory,{memory_subtype}"
        args.tags = f"{args.tags},{injected}" if args.tags else injected
        filetags = build_filetags(args.tags, ancestor_slugs)

    # component: derives short name from title and falls back brief to description.
    # entity_org / field_group / junction: carry #+brief: from --brief only
    # (no description fallback — description already lands in #+description:).
    if args.type == "component":
        component_name = (
            args.title[len("ores."):] if args.title.startswith("ores.")
            else args.title
        )
        component_brief = args.brief or args.description
    elif args.type in ("entity_org", "field_group", "junction"):
        component_name = ""
        component_brief = args.brief or ""
    else:
        component_name = ""
        component_brief = ""

    # entity_plural defaults to slug + 's' when not supplied for types that use it.
    _PLURAL_TYPES = ("entity_org", "table", "lookup_entity", "junction")
    if args.type in _PLURAL_TYPES:
        entity_plural = args.entity_plural or (args.slug + "s")
        has_tenant_id = args.has_tenant_id if args.type != "entity_org" else ""
        coding_scheme = args.coding_scheme if args.type == "table" else ""
    else:
        entity_plural = ""
        has_tenant_id = ""
        coding_scheme = ""

    # entity_title / name_title: title-case version of slug.
    if args.type == "entity_org":
        entity_title = " ".join(w.capitalize() for w in args.slug.split("_"))
        name_title = ""
    elif args.type == "junction":
        entity_title = ""
        name_title = " ".join(w.capitalize() for w in args.slug.split("_"))
    else:
        entity_title = ""
        name_title = ""

    # dataset_overview carries dataset-specific metadata keywords.
    if args.type == "dataset_overview":
        dataset_name = args.dataset
        dataset_version = args.dataset_version
        dataset_type = args.dataset_type
        source_methodology = args.source_methodology
    else:
        dataset_name = ""
        dataset_version = ""
        dataset_type = ""
        source_methodology = ""

    goal_default = {
        "task": "(Describe what user-visible-or-internal change this "
                "task produces.)",
        "story": "(Describe the user-visible outcome this story "
                 "delivers.)",
    }.get(args.type, "")

    # Recipe body sections: rendered verbatim when supplied, fill-in
    # placeholders otherwise, so a fully-argumented recipe needs no
    # post-generation editing.
    recipe_intro = args.intro or (
        "(One- or two-sentence pointer to the component model and any "
        "knowledge\ndoc this recipe relies on.)")
    recipe_question = args.question or (
        "(Restate the NLP question this recipe answers.)")
    recipe_answer = args.answer or (
        "#+begin_src sh :results verbatim\n"
        "# Replace with the command(s) that answer the question.\n"
        "#+end_src")
    recipe_script = args.script or (
        "(Pointer to the script or wrapper that does the work, if "
        "applicable.)")
    recipe_tested_by = args.tested_by or (
        "(How this recipe is exercised — CI workflow, manual smoke "
        "test, etc.)")

    variables = {
        "id": new_id,
        "goal": args.goal or goal_default,
        "acceptance": args.acceptance,
        "has_acceptance": bool(args.acceptance),
        "intro": recipe_intro,
        "question": recipe_question,
        "answer": recipe_answer,
        "script": recipe_script,
        "tested_by": recipe_tested_by,
        "see_also": args.see_also,
        "has_see_also": bool(args.see_also),
        "slug": args.slug,
        "title": args.title,
        "description": args.description,
        "filetags": filetags,
        "owner": args.owner or "marco",
        "date": today,
        "state": state,
        "parent_dir": str(parent_dir),
        "parent_id": (args.parent_id or "").upper(),
        "parent_title": args.parent_title,
        "predecessor_id": (args.predecessor_id or "").upper(),
        "predecessor_title": args.predecessor_title or "",
        "bucket": bucket,
        "memory_subtype": memory_subtype,
        "statement": args.statement or "(One short paragraph stating the "
                     "rule, fact, or insight to remember.)",
        "why": args.why or "(The reason the user gave, or the incident this "
               "captures. Knowing the why lets a future session judge edge "
               "cases instead of blindly applying the rule.)",
        "how_to_apply": args.how_to_apply or "(When this memory should kick "
                        "in — what circumstances, which files, which "
                        "workflows.)",
        "component": args.component,
        "facet_group": args.facet_group,
        "component_name": component_name,
        "brief": component_brief,
        "entity_plural": entity_plural,
        "entity_title": entity_title,
        "name_title": name_title,
        "has_tenant_id": has_tenant_id,
        "coding_scheme": coding_scheme,
        "dataset_name": dataset_name,
        "dataset_version": dataset_version,
        "dataset_type": dataset_type,
        "source_methodology": source_methodology,
        "end_date": (date.today() + timedelta(days=7)).isoformat(),
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
    # - entity_org:    <parent-dir>/ores.<component>.<slug>.org
    # - field_group:   <parent-dir>/ores.<component>.<slug>_field_group.org
    # - table:         <parent-dir>/ores.<component>.<slug>_table.org
    # - junction:      <parent-dir>/ores.<component>.<slug>_junction.org
    # - lookup_entity: <parent-dir>/ores.<component>.<slug>_lookup_entity.org
    # - service_registry: <parent-dir>/service_registry.org
    # - story / sprint / version: <parent-dir>/<slug>/<type>.org
    #   (these are composition nodes; they hold children)
    if args.type == "task":
        # Don't double-prefix if the caller already passed task_<slug>.
        leaf = args.slug if args.slug.startswith("task_") else f"task_{args.slug}"
        out_dir = parent_dir
        out_file = out_dir / f"{leaf}.org"
    elif args.type == "entity_org":
        out_dir = parent_dir
        out_file = out_dir / f"ores.{args.component}.{args.slug}.org"
    elif args.type == "field_group":
        # Suffix matters: the codegen loader dispatches on *_field_group.org.
        out_dir = parent_dir
        out_file = out_dir / f"ores.{args.component}.{args.slug}_field_group.org"
    elif args.type == "table":
        # Suffix matters: the codegen loader dispatches on *_table.org.
        out_dir = parent_dir
        out_file = out_dir / f"ores.{args.component}.{args.slug}_table.org"
    elif args.type == "junction":
        # Suffix matters: the codegen loader dispatches on *_junction.org.
        out_dir = parent_dir
        out_file = out_dir / f"ores.{args.component}.{args.slug}_junction.org"
    elif args.type == "lookup_entity":
        # Suffix matters: the codegen loader dispatches on *_lookup_entity.org.
        out_dir = parent_dir
        out_file = out_dir / f"ores.{args.component}.{args.slug}_lookup_entity.org"
    elif args.type == "service_registry":
        out_dir = parent_dir
        out_file = out_dir / "service_registry.org"
    elif args.type in ("facet", "technical_space", "archetype"):
        # Physical-space graph nodes are flat files named for their address:
        # the slug IS the address (ores.cpp / ores.cpp.qt / ores.cpp.qt.controller).
        out_dir = parent_dir
        out_file = out_dir / f"{args.slug}.org"
    elif args.type == "facet_group":
        # Suffix keeps a group doc from colliding with a same-named facet
        # (e.g. cmake.org facet vs cmake_group.org group).
        out_dir = parent_dir
        out_file = out_dir / f"{args.slug}_group.org"
    elif args.type == "dataset_overview":
        out_dir = parent_dir
        out_file = out_dir / "dataset_overview.org"
    elif args.type in ("component", "recipe", "knowledge", "manual", "product_identity",
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
