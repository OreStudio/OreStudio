from dataclasses import dataclass
from typing import Dict, List


@dataclass
class Component:
    name: str
    models_dir: str
    entity_glob: str = "*_entity.json"
    # *_entity.json also matches *_domain_entity.json; exclude the latter so
    # regenerate only processes pure schema models, not domain entity models.
    exclude_suffix: str = "_domain_entity.json"


COMPONENTS: Dict[str, Component] = {
    "refdata": Component(
        name="refdata",
        models_dir="projects/ores.codegen/models/refdata",
    ),
    "trade": Component(
        name="trade",
        models_dir="projects/ores.codegen/models/trade",
    ),
    "dq": Component(
        name="dq",
        models_dir="projects/ores.codegen/models/dq",
    ),
    "iam": Component(
        name="iam",
        models_dir="projects/ores.codegen/models/iam",
    ),
}


def get_component(name: str) -> Component:
    if name not in COMPONENTS:
        known = ", ".join(sorted(COMPONENTS))
        raise ValueError(f"Unknown component: {name!r}. Known components: {known}")
    return COMPONENTS[name]


def all_components() -> List[str]:
    return list(COMPONENTS.keys())
