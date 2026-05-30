from dataclasses import dataclass
from typing import Dict, List, Optional


@dataclass
class Component:
    name: str
    models_dir: str
    entity_glob: str = "*_entity.json"
    # *_entity.json also matches *_domain_entity.json; exclude the latter so
    # regenerate only processes pure schema models, not domain entity models.
    # None means no exclusion.
    exclude_suffix: Optional[str] = "_domain_entity.json"


COMPONENTS: Dict[str, Component] = {
    # SQL schema components (--profile sql)
    "refdata": Component(
        name="refdata",
        models_dir="projects/ores.codegen/models/refdata",
        entity_glob="*_table.json",
        exclude_suffix="_domain_entity.json",
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
    # C++ domain entity components (--profile all-cpp)
    "analytics-cpp": Component(
        name="analytics-cpp",
        models_dir="projects/ores.codegen/models/analytics",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
    ),
    "compute-cpp": Component(
        name="compute-cpp",
        models_dir="projects/ores.codegen/models/compute",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
    ),
    "controller-cpp": Component(
        name="controller-cpp",
        models_dir="projects/ores.codegen/models/controller",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
    ),
    "database-cpp": Component(
        name="database-cpp",
        models_dir="projects/ores.codegen/models/database",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
    ),
    "dq-cpp": Component(
        name="dq-cpp",
        models_dir="projects/ores.codegen/models/dq",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
    ),
    "iam-cpp": Component(
        name="iam-cpp",
        models_dir="projects/ores.codegen/models/iam",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
    ),
    "refdata-cpp": Component(
        name="refdata-cpp",
        models_dir="projects/ores.codegen/models/refdata",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
    ),
    "reporting-cpp": Component(
        name="reporting-cpp",
        models_dir="projects/ores.codegen/models/reporting",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
    ),
    "scheduler-cpp": Component(
        name="scheduler-cpp",
        models_dir="projects/ores.codegen/models/scheduler",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
    ),
    "trading-cpp": Component(
        name="trading-cpp",
        models_dir="projects/ores.codegen/models/trading",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
    ),
    "workflow-cpp": Component(
        name="workflow-cpp",
        models_dir="projects/ores.codegen/models/workflow",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
    ),
    "workspace-cpp": Component(
        name="workspace-cpp",
        models_dir="projects/ores.codegen/models/workspace",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
    ),
}


def get_component(name: str) -> Component:
    if name not in COMPONENTS:
        known = ", ".join(sorted(COMPONENTS))
        raise ValueError(f"Unknown component: {name!r}. Known components: {known}")
    return COMPONENTS[name]


def all_components() -> List[str]:
    return list(COMPONENTS.keys())
