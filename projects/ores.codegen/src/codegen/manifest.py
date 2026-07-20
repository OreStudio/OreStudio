import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Union

_COMPONENT_CATALOGUE = Path(__file__).parent.parent.parent / "library" / "component_catalogue.org"


@dataclass
class Component:
    name: str
    # Legacy JSON discovery root. "" (empty, not None) for org-only
    # components like refdata, which have no legacy JSON path at all.
    models_dir: str
    # Either a single glob pattern or a tuple of patterns (the regenerate
    # driver unions the matches). Tuple form supports the org-mode model
    # POC (=*_entity.org=) alongside the legacy JSON models.
    entity_glob: Union[str, Tuple[str, ...]] = "*_entity.json"
    # *_entity.json also matches *_domain_entity.json; exclude the latter so
    # regenerate only processes pure schema models, not domain entity models.
    # None means no exclusion.
    exclude_suffix: Optional[str] = "_domain_entity.json"
    # Optional second discovery root: the component's own modeling/ dir
    # (e.g. projects/ores.refdata/modeling/). Any *.org file in this dir
    # whose frontmatter declares `#+type: ores.codegen.entity` is treated
    # as an entity model. Other org files (component overviews, plantuml
    # source, knowledge docs) are skipped. None means no second root.
    modeling_dir: Optional[str] = None
    # Codegen org types to skip within modeling_dir even though they
    # otherwise qualify (e.g. "junction" while junction codegen is still
    # being brought up to parity with entity codegen, or "module" since
    # it is a docs-only index, never a generation target). Values are the
    # short suffix after "ores.codegen." (see _CODEGEN_ORG_TYPES). Empty
    # means no exclusion.
    exclude_org_types: Tuple[str, ...] = ()


def _load_components() -> Dict[str, "Component"]:
    text = _COMPONENT_CATALOGUE.read_text(encoding="utf-8")
    lines = text.splitlines()

    table_lines = []
    in_section = False
    in_table = False
    for line in lines:
        stripped = line.strip()
        if stripped == "* Components":
            in_section = True
            continue
        if in_section and stripped.startswith("* "):
            break
        if not in_table:
            if in_section and stripped.startswith("| name"):
                in_table = True
                table_lines.append(line)
        else:
            if stripped.startswith("|"):
                table_lines.append(line)
            else:
                break

    if not table_lines:
        raise ValueError(f"No component table found in {_COMPONENT_CATALOGUE}")

    rows = []
    for line in table_lines:
        stripped = line.strip()
        if re.match(r"^\|[-+|]+\|?$", stripped):
            continue
        rows.append([c.strip() for c in stripped.strip("|").split("|")])

    if len(rows) < 2:
        raise ValueError(f"Empty component table in {_COMPONENT_CATALOGUE}")

    headers = [h.lower() for h in rows[0]]
    result: Dict[str, Component] = {}
    for row in rows[1:]:
        entry = dict(zip(headers, row))
        name = entry["name"]
        exclude_org_types = entry.get("exclude_org_types") or ""
        result[name] = Component(
            name=name,
            models_dir=entry["models_dir"],
            entity_glob=entry.get("entity_glob") or "*_entity.json",
            exclude_suffix=entry.get("exclude_suffix") or None,
            modeling_dir=entry.get("modeling_dir") or None,
            exclude_org_types=tuple(
                t.strip() for t in exclude_org_types.split(",") if t.strip()
            ),
        )
    return result


COMPONENTS: Dict[str, Component] = _load_components()


# Filter for org files in a component's modeling/ dir: only files whose
# frontmatter declares a codegen model type are picked up. Other org
# files (overviews, knowledge docs, plantuml source) are skipped.
_ORG_TYPE_RE = re.compile(r"^#\+type:\s*(\S+)\s*$", re.MULTILINE | re.IGNORECASE)
_CODEGEN_ORG_TYPES = frozenset({
    "ores.codegen.entity",
    "ores.codegen.field_group",
    "ores.codegen.junction",
    "ores.codegen.lookup_entity",
    "ores.codegen.service_registry",
    "ores.codegen.component",
})


def _org_type(path: Path) -> Optional[str]:
    """The file's `#+type:` frontmatter value, or None if absent."""
    with path.open(encoding="utf-8", errors="replace") as f:
        head = f.read(4096)
    match = _ORG_TYPE_RE.search(head)
    return match.group(1) if match else None


def is_codegen_entity_org(path: Path) -> bool:
    """True if the file's frontmatter declares it a codegen org-model."""
    org_type = _org_type(path)
    return bool(org_type and org_type in _CODEGEN_ORG_TYPES)


def discover_models(
    comp: Component, project_root: Path, apply_exclusions: bool = True
) -> List[Path]:
    """All model files for a component, across both discovery roots.

    The legacy JSON root (models_dir) may no longer exist — the org
    migration deletes JSON models as it converts them — so its absence
    is not an error. Org files under modeling_dir are picked up when
    their frontmatter declares a codegen model type.

    apply_exclusions gates comp.exclude_org_types. It defaults to True
    for the bulk `codegen regenerate --component` path, which is what
    exclude_org_types exists to scope (e.g. skipping junction-typed
    files whose bulk regeneration currently errors on some addresses).
    Single-entity commands (codegen entity list/generate/show/diff) must
    still be able to resolve those same files directly, so callers on
    that path pass apply_exclusions=False.
    """
    matches: set = set()
    if comp.models_dir and (project_root / comp.models_dir).is_dir():
        models_dir = project_root / comp.models_dir
        globs = (
            (comp.entity_glob,)
            if isinstance(comp.entity_glob, str)
            else tuple(comp.entity_glob)
        )
        for pattern in globs:
            matches.update(models_dir.glob(pattern))
        matches = {
            f for f in matches
            if comp.exclude_suffix is None
            or not f.name.endswith(comp.exclude_suffix)
        }
    if comp.modeling_dir:
        modeling_dir = project_root / comp.modeling_dir
        if modeling_dir.is_dir():
            for org_path in modeling_dir.glob("*.org"):
                if not org_path.is_file():
                    continue
                org_type = _org_type(org_path)
                if org_type not in _CODEGEN_ORG_TYPES:
                    continue
                short_type = org_type.removeprefix("ores.codegen.")
                if apply_exclusions and short_type in comp.exclude_org_types:
                    continue
                matches.add(org_path)
    return sorted(matches)


def get_component(name: str) -> Component:
    if name not in COMPONENTS:
        known = ", ".join(sorted(COMPONENTS))
        raise ValueError(f"Unknown component: {name!r}. Known components: {known}")
    return COMPONENTS[name]


def all_components() -> List[str]:
    return list(COMPONENTS.keys())
