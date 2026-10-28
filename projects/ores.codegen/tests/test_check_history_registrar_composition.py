"""Tests that the history-provider composition check finds a registrar nothing
composes, and that its --fix mode wires one without disturbing its neighbours.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_check_history_registrar_composition.py
"""
import importlib.util
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SCRIPT = (
    REPO_ROOT
    / "projects/ores.codegen/scripts/check_history_registrar_composition.py"
)

spec = importlib.util.spec_from_file_location("check_history_registrars", SCRIPT)
check = importlib.util.module_from_spec(spec)
spec.loader.exec_module(check)

HEADER = """#ifndef ORES_REFDATA_CORE_MESSAGING_ALPHA_HISTORY_PROVIDER_REGISTRAR_HPP
#define ORES_REFDATA_CORE_MESSAGING_ALPHA_HISTORY_PROVIDER_REGISTRAR_HPP

namespace ores::refdata::messaging {

void register_alpha_history_provider(
    ores::history::service::dispatch_registry& registry);

}

#endif
"""

COMPOSITION = """#include "ores.refdata.core/messaging/alpha_history_provider_registrar.hpp"
#include "ores.refdata.core/messaging/alpha_registrar.hpp"
#include "ores.refdata.core/messaging/beta_history_provider_registrar.hpp"
#include "ores.refdata.core/messaging/beta_registrar.hpp"

namespace ores::refdata::messaging {

ores::history::service::dispatch_registry& history_registry() {
    static ores::history::service::dispatch_registry instance;
    return instance;
}

void compose() {
    auto& hist_registry = history_registry();
    {
        register_beta_history_provider(hist_registry);
    }
}

}
"""


def component(tmp_path, composition=COMPOSITION, entities=("alpha", "beta")) -> Path:
    """A component with a registrar header for each of entities."""
    root = tmp_path / "ores.refdata"
    header_dir = root / "core/include/ores.refdata.core/messaging"
    header_dir.mkdir(parents=True)
    for entity in entities:
        (header_dir / f"{entity}_history_provider_registrar.hpp").write_text(
            HEADER.replace("alpha", entity), encoding="utf-8")
    if composition is not None:
        source = root / "core/src/messaging"
        source.mkdir(parents=True)
        (source / "registrar.cpp").write_text(composition, encoding="utf-8")
    return root


def test_a_registrar_nothing_composes_is_reported(tmp_path):
    failure = check.check_component(component(tmp_path))
    assert failure is not None
    assert "alpha" in failure and "not composed" in failure
    assert "beta" not in failure


def test_a_component_with_no_composition_file_is_reported(tmp_path):
    failure = check.check_component(component(tmp_path, composition=None))
    assert failure is not None
    assert "no core/src/messaging/registrar.cpp" in failure


def test_a_component_with_no_registrars_is_not_reported(tmp_path):
    root = tmp_path / "ores.empty"
    (root / "core/include").mkdir(parents=True)
    assert check.check_component(root) is None


def test_the_fix_wires_the_registrar_and_keeps_the_include_block_sorted(tmp_path):
    root = component(tmp_path)
    assert check.fix_component(root) == ["alpha"]

    written = (root / "core/src/messaging/registrar.cpp").read_text()
    assert "        register_alpha_history_provider(hist_registry);\n" in written
    assert "        register_beta_history_provider(hist_registry);\n" in written
    includes = [line for line in written.splitlines()
                if line.endswith("_registrar.hpp\"")]
    assert includes == sorted(includes)
    assert check.check_component(root) is None


def test_the_fix_writes_nothing_when_every_registrar_is_composed(tmp_path):
    root = component(tmp_path, COMPOSITION.replace(
        "        register_beta_history_provider(hist_registry);\n",
        "        register_alpha_history_provider(hist_registry);\n"
        "        register_beta_history_provider(hist_registry);\n"))
    assert check.fix_component(root) == []


def test_the_tree_composes_every_registrar_of_a_checked_component():
    assert check.main() == 0
