"""Tests for the junction messaging-facet gate in ``resolve_targets``.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_junction_messaging_gate.py

A junction's messaging layer exists to serve parent-scoped list reads
(``:list_by:`` on a junction side). The gate in ``resolve_targets`` drops
the messaging facets for a junction whose left and right sides both lack
``:list_by:``, so no regenerated stack sits without a subscriber.

Both sides are pinned. The declaring side uses a live org -- the dq
``dataset_bundle_member`` junction declares ``:list_by:`` on its left
side. The bare side is a fixture rather than a live org, because every
live refdata junction now declares a read; a fixture keeps this test
about the gate instead of about whichever org happens to declare nothing
today.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DECLARING_JUNCTION = (
    REPO_ROOT / "projects/ores.dq/modeling/ores.dq.dataset_bundle_member_junction.org"
)

# The six archetypes the four C++ messaging facets resolve for a junction:
# handler, registrar header/impl, protocol header, service header/impl.
MESSAGING_TEMPLATES = frozenset({
    "cpp_nats_handler.hpp.mustache",
    "cpp_nats_registrar.hpp.mustache",
    "cpp_nats_registrar.cpp.mustache",
    "cpp_protocol.hpp.mustache",
    "cpp_service.hpp.mustache",
    "cpp_service.cpp.mustache",
})

# The TypeScript twin of the protocol header. It is part of the same
# decision: a junction either renders both halves of the wire shape or
# neither, never a TypeScript module with no C++ header to pair it with.
TS_PROTOCOL_TEMPLATE = "ts_protocol.ts.mustache"

# A junction with no side declaring ``:list_by:``. Field names are
# arbitrary; the gate reads only the two side drawers.
BARE_JUNCTION = """\
:PROPERTIES:
:END:
#+title: ores.refdata.test_bare_junction
#+type: ores.codegen.junction
#+component: refdata
#+filetags: :model:junction:refdata:
#+name: test_bare_junctions
#+name_singular: test_bare_junction
#+name_title: Test Bare Junction
#+name_singular_words: test bare junction
#+brief: Fixture for the messaging gate.
#+product: ores
#+schema: public
#+has_tenant_id: true

* Flags
:PROPERTIES:
:profile: tenant-scoped-junction
:END:

* Left
:PROPERTIES:
:column:        left_code
:column_short:  left
:column_title:  Left
:type:          text
:cpp_type:      std::string
:index_comment: Index.
:END:

* Right
:PROPERTIES:
:column:        right_code
:column_short:  right
:column_title:  Right
:type:          text
:cpp_type:      std::string
:index_comment: Index.
:END:

* Columns

* SQL

** Flags
:PROPERTIES:
:tablename: ores_refdata_test_bare_junctions_tbl
:END:

* Repository
:PROPERTIES:
:name_singular_short: test_bare_junction
:name_short:          test_bare_junctions
:name_singular_words: test bare junction
:name_words:          test bare junctions
:order_column:        right_code
:END:

* C++

** Flags
:PROPERTIES:
:subcomponent: api
:END:
"""


def test_list_by_declaring_junction_keeps_full_messaging_stack():
    units, model_type, _ = resolve_targets(DECLARING_JUNCTION, CODEGEN_BASE)
    assert model_type == "junction"
    templates = {u["template"] for u in units}
    assert MESSAGING_TEMPLATES <= templates
    assert TS_PROTOCOL_TEMPLATE in templates


def test_bare_junction_resolves_no_messaging_and_keeps_its_stack(tmp_path):
    bare = tmp_path / "ores.refdata.test_bare_junction.org"
    bare.write_text(BARE_JUNCTION, encoding="utf-8")

    bare_units, _, _ = resolve_targets(bare, CODEGEN_BASE)
    templates = {u["template"] for u in bare_units}
    assert not (MESSAGING_TEMPLATES & templates)
    assert TS_PROTOCOL_TEMPLATE not in templates
    # The gate must not over-drop: a bare junction still resolves its
    # non-messaging stack, e.g. the SQL create and its TypeScript domain.
    assert "sql_schema_junction_create.mustache" in templates
    assert "domain_types.ts.mustache" in templates
