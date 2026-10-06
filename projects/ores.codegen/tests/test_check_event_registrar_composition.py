"""Tests that the event-registrar composition check finds a registrar nothing
composes, and passes when the composition calls every one.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_check_event_registrar_composition.py
"""
import importlib.util
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SCRIPT = REPO_ROOT / "projects/ores.codegen/scripts/check_event_registrar_composition.py"

spec = importlib.util.spec_from_file_location("check_event_registrars", SCRIPT)
check = importlib.util.module_from_spec(spec)
spec.loader.exec_module(check)

HEADER = """#ifndef ORES_REFDATA_SERVICE_MESSAGING_ALPHA_EVENT_REGISTRAR_HPP
#define ORES_REFDATA_SERVICE_MESSAGING_ALPHA_EVENT_REGISTRAR_HPP

namespace ores::refdata::service::messaging {

[[nodiscard]] ores::eventing::service::subscription register_alpha_event_mapping(
    ores::eventing::service::postgres_event_source& event_source,
    ores::eventing::service::event_bus& event_bus,
    ores::nats::service::client& nats);

}

#endif
"""


def component(tmp_path, composition: str | None) -> Path:
    root = tmp_path / "ores.refdata"
    header_dir = root / "service/include/ores.refdata.service/messaging"
    header_dir.mkdir(parents=True)
    (header_dir / "alpha_event_registrar.hpp").write_text(HEADER, encoding="utf-8")
    if composition is not None:
        source = root / "service/src/messaging"
        source.mkdir(parents=True)
        (source / "event_registrar.cpp").write_text(composition, encoding="utf-8")
    return root


def test_a_registrar_nothing_composes_is_reported(tmp_path):
    failure = check.check_component(component(tmp_path, "int main() { return 0; }\n"))
    assert failure is not None
    assert "alpha" in failure and "not composed" in failure


def test_a_composed_registrar_passes(tmp_path):
    composition = (
        "subs.push_back(register_alpha_event_mapping(event_source, event_bus, nats));\n"
    )
    assert check.check_component(component(tmp_path, composition)) is None


def test_a_component_with_no_composition_file_is_reported(tmp_path):
    failure = check.check_component(component(tmp_path, None))
    assert failure is not None
    assert "no service/src/messaging/event_registrar.cpp" in failure


def test_a_component_with_no_registrars_is_not_reported(tmp_path):
    root = tmp_path / "ores.empty"
    (root / "service/include").mkdir(parents=True)
    assert check.check_component(root) is None


def test_the_tree_composes_every_registrar_of_a_checked_component():
    assert check.main() == 0
