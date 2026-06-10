import json
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Union

_COMPONENTS_JSON = Path(__file__).parent.parent.parent / "library" / "components.json"


@dataclass
class Component:
    name: str
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


def _load_components() -> Dict[str, "Component"]:
    with _COMPONENTS_JSON.open(encoding="utf-8") as f:
        data = json.load(f)
    result: Dict[str, Component] = {}
    for key, entry in data["components"].items():
        result[key] = Component(
            name=key,
            models_dir=entry["models_dir"],
            entity_glob=entry.get("entity_glob", "*_entity.json"),
            exclude_suffix=entry.get("exclude_suffix", "_domain_entity.json"),
            modeling_dir=entry.get("modeling_dir"),
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
    "ores.codegen.table",
    "ores.codegen.lookup_entity",
    "ores.codegen.service_registry",
    "ores.codegen.component",
})


def is_codegen_entity_org(path: Path) -> bool:
    """True if the file's frontmatter declares it a codegen org-model."""
    with path.open(encoding="utf-8", errors="replace") as f:
        head = f.read(4096)
    match = _ORG_TYPE_RE.search(head)
    return bool(match and match.group(1) in _CODEGEN_ORG_TYPES)


def discover_models(comp: Component, project_root: Path) -> List[Path]:
    """All model files for a component, across both discovery roots.

    The legacy JSON root (models_dir) may no longer exist — the org
    migration deletes JSON models as it converts them — so its absence
    is not an error. Org files under modeling_dir are picked up when
    their frontmatter declares a codegen model type.
    """
    matches: set = set()
    models_dir = project_root / comp.models_dir
    if models_dir.is_dir():
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
                if org_path.is_file() and is_codegen_entity_org(org_path):
                    matches.add(org_path)
    return sorted(matches)


def get_component(name: str) -> Component:
    if name not in COMPONENTS:
        known = ", ".join(sorted(COMPONENTS))
        raise ValueError(f"Unknown component: {name!r}. Known components: {known}")
    return COMPONENTS[name]


def all_components() -> List[str]:
    return list(COMPONENTS.keys())
