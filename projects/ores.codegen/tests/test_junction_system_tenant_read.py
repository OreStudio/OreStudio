"""A global-registry junction reads its own tenant's rows and the system's.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_junction_system_tenant_read.py

A junction that hangs off a ``:system_tenant_visible:`` entity carries the
platform-managed half of that registry: compute's app version platforms own
the per-platform package the platform publishes under the system tenant.
Its row-level policy exposes system rows to every tenant, so a read narrowed
to the caller's own tenant would hide exactly the package a dispatch needs.

``:system_tenant_visible: true`` in the junction's Repository drawer widens
every *read* where-clause to own-tenant OR system-tenant. A mutation must not
widen: the policy's ``with check`` keeps a tenant writing its own rows, and a
delete that matched system rows would let one tenant remove the platform's
package for all of them.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"
VISIBLE_JUNCTION = (
    REPO_ROOT
    / "projects/ores.compute/modeling/ores.compute.app_version_platform_junction.org"
)


def _render(tmp_path) -> str:
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    output_name = "app_version_platform_repository.cpp"
    generate_from_model(
        str(VISIBLE_JUNCTION),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_domain_type_repository.cpp.mustache",
        target_output=output_name,
    )
    return (output_dir / output_name).read_text(encoding="utf-8")


def _body_after(sql: str, marker: str) -> str:
    start = sql.index(marker)
    return sql[start:start + 2000]


def test_a_read_admits_the_system_tenants_rows(tmp_path):
    sql = _render(tmp_path)

    body = _body_after(
        sql, "app_version_platform_repository::read_latest_by_app_version(")

    assert '"tenant_id"_c == sys' in body


def test_a_removal_stays_scoped_to_the_writers_own_tenant(tmp_path):
    sql = _render(tmp_path)

    body = _body_after(sql, "app_version_platform_repository::remove(")

    assert '"tenant_id"_c == tid' in body
    assert '"tenant_id"_c == sys' not in body
