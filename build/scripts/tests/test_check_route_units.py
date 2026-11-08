"""The route-unit check catches a unit the aggregate never registers.

Run::

    python3 -m pytest build/scripts/tests/test_check_route_units.py

The trap is silent: a route unit the aggregate leaves out compiles, links and passes
its own cases, and the gateway answers nothing for it. The check has to name the unit
and the model that opted in, and it must not invent a problem for a component that is
in order.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "build/scripts"))

import check_route_units  # noqa: E402


def _write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def _aggregate(root: Path, component: str, units) -> None:
    header = f'#include "ores.http/routes/{component}/{component}_routes.hpp"\n'
    includes = "".join(
        f'#include "ores.http/routes/{component}/{unit}_routes.hpp"\n' for unit in units)
    calls = "".join(
        f"    {unit}_routes::register_routes(router, registry, session);\n" for unit in units)
    _write(root / f"projects/ores.http/{component}/src/routes/{component}/"
                  f"{component}_routes.cpp",
           header + includes + "void register_all() {\n" + calls + "}\n")


def _unit(root: Path, component: str, unit: str) -> None:
    _write(root / f"projects/ores.http/{component}/src/routes/{component}/{unit}_routes.cpp",
           "void register_routes() {}\n")


def _model(root: Path, component: str, name: str, *, opted_in: bool,
           keyword: str = "entity_singular") -> None:
    flag = ":ores.cpp.http-route.enabled: true\n" if opted_in else ""
    _write(root / f"projects/ores.{component}/modeling/ores.{component}.{name}.org",
           ":PROPERTIES:\n" + flag + ":END:\n#+type: ores.codegen.entity\n"
           f"#+{keyword}: {name}\n")


def _tree(tmp_path: Path) -> Path:
    _aggregate(tmp_path, "demo", ["alpha", "beta"])
    _unit(tmp_path, "demo", "alpha")
    _unit(tmp_path, "demo", "beta")
    _model(tmp_path, "demo", "alpha", opted_in=True)
    _model(tmp_path, "demo", "beta", opted_in=True)
    return tmp_path


def test_an_aggregate_that_registers_every_opted_in_unit_is_clean(tmp_path):
    problems, checked = check_route_units.check(_tree(tmp_path))
    assert problems == []
    assert checked == 1


def test_a_unit_the_aggregate_never_registers_is_named(tmp_path):
    root = _tree(tmp_path)
    _aggregate(root, "demo", ["alpha"])
    problems, _ = check_route_units.check(root)
    assert len(problems) == 1
    assert "never registers beta_routes" in problems[0]
    assert "ores.demo.beta.org opts into" in problems[0]


def test_a_model_that_did_not_opt_in_is_not_expected(tmp_path):
    root = _tree(tmp_path)
    _aggregate(root, "demo", ["alpha"])
    _model(root, "demo", "beta", opted_in=False)
    problems, _ = check_route_units.check(root)
    assert problems == []


def test_a_junction_names_its_unit_by_its_singular(tmp_path):
    root = _tree(tmp_path)
    _model(root, "demo", "gamma", opted_in=True, keyword="name_singular")
    problems, _ = check_route_units.check(root)
    assert len(problems) == 1
    assert "never registers gamma_routes" in problems[0]


def test_a_unit_registered_twice_is_a_problem(tmp_path):
    root = _tree(tmp_path)
    _aggregate(root, "demo", ["alpha", "beta", "beta"])
    problems, _ = check_route_units.check(root)
    assert any("registers beta_routes 2 times" in problem for problem in problems)
