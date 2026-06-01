from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple, Union


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
        modeling_dir="projects/ores.analytics/modeling",
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
        modeling_dir="projects/ores.dq/modeling",
    ),
    "iam-cpp": Component(
        name="iam-cpp",
        models_dir="projects/ores.codegen/models/iam",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
        modeling_dir="projects/ores.iam/modeling",
    ),
    "refdata-cpp": Component(
        name="refdata-cpp",
        models_dir="projects/ores.codegen/models/refdata",
        entity_glob="*_domain_entity.json",
        exclude_suffix=None,
        modeling_dir="projects/ores.refdata/modeling",
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
        modeling_dir="projects/ores.trading/modeling",
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
