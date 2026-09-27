"""Regression tests for the B04 subject census in survey_component.py.

``scripts/survey_component.py`` built its subject pattern from the project
directory name, so ``ores.http`` became ``http`` and the header declaring
``http-server.v1.info.get`` was reported as zero subjects. The header census
now reads declarations by shape, which cannot be fooled by a token that
differs from the directory name.

The differing-token cases run against a throw-away component written to a
temp directory, so the tree gains no fake component. The last tests read the
real ``ores.http`` header, so the fix is proven on live data too.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_survey_component_subjects.py
"""
import importlib.util
import sys
import tempfile
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
LEVER = REPO_ROOT / "projects" / "ores.codegen" / "scripts" / "survey_component.py"

spec = importlib.util.spec_from_file_location("survey_component", LEVER)
survey = importlib.util.module_from_spec(spec)
sys.modules["survey_component"] = survey
spec.loader.exec_module(survey)

HTTP_HEADER = "api/include/ores.http.api/messaging/http_info_protocol.hpp"
IAM_HEADER = "api/include/ores.iam.api/messaging/account_protocol.hpp"

# The defect: ores.http declares a subject whose first token is http-server,
# not the directory token http, so the directory-token scan found nothing.
HTTP_SERVER_BODY = """\
#include <string_view>

namespace ores::http::messaging {

struct get_http_info_request {
    static constexpr std::string_view nats_subject = "http-server.v1.info.get";
};

}
"""

# A declaration whose token matches the directory name: the token scan
# already found this, and it must keep finding it exactly once.
IAM_BODY = """\
#include <string_view>

namespace ores::iam::messaging {

struct list_accounts_request {
    static constexpr std::string_view nats_subject = "iam.v1.accounts.list";
};

}
"""


@pytest.fixture
def scratch():
    """A temp directory outside the repo for throw-away components."""
    with tempfile.TemporaryDirectory() as directory:
        yield Path(directory)


def _write(path: Path, body: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body, encoding="utf-8")


def _component(scratch: Path, name: str, header: str, body: str) -> Path:
    """A throw-away component holding only the header a test needs."""
    project = scratch / name
    _write(project / header, body)
    return project


def _header_row(lines, filename):
    """The protocol-headers table cells for one header."""
    start = lines.index("### Protocol headers")
    for line in lines[start:]:
        if line.startswith("| ") and filename in line:
            return [cell.strip() for cell in line.strip("|").split("|")]
    raise AssertionError(f"no protocol header row for {filename}")


def test_a_declared_subject_is_read_when_its_token_differs(scratch):
    project = _component(scratch, "ores.http", HTTP_HEADER, HTTP_SERVER_BODY)

    declared = survey.declared_subjects(project / HTTP_HEADER)

    assert declared == [(6, "http-server.v1.info.get")]


def test_a_declared_subject_reaches_the_source_literal_census(scratch):
    project = _component(scratch, "ores.http", HTTP_HEADER, HTTP_SERVER_BODY)

    census = survey.component_subjects(project)

    assert census == [(project / HTTP_HEADER, 6, "http-server.v1.info.get")]


def test_the_protocol_header_subject_count_is_one(scratch):
    project = _component(scratch, "ores.http", HTTP_HEADER, HTTP_SERVER_BODY)

    lines = survey.report_protocol(project, project / "modeling")

    assert _header_row(lines, "http_info_protocol.hpp") == [
        str(project / HTTP_HEADER), "hand-written", "1"]


def test_a_token_matching_declaration_is_found_once(scratch):
    project = _component(scratch, "ores.iam", IAM_HEADER, IAM_BODY)

    census = survey.component_subjects(project)

    assert census == [(project / IAM_HEADER, 6, "iam.v1.accounts.list")]


def test_a_token_matching_literal_that_is_not_a_declaration_is_still_found(scratch):
    project = scratch / "ores.iam"
    route = project / "api/src/routes/account_routes.cpp"
    _write(route, 'nats.subscribe("iam.v1.accounts.list");\n')

    census = survey.component_subjects(project)

    assert census == [(route, 1, "iam.v1.accounts.list")]


def test_the_real_http_header_reports_its_declared_subject():
    header = (REPO_ROOT / "projects/ores.http/api/include/ores.http.api"
              / "messaging/http_info_protocol.hpp")

    declared = survey.declared_subjects(header)

    # The clean-standard pass renamed the subject to the component token; the
    # declaration shape is unchanged, so the live header yields exactly one
    # subject and it is this literal.
    assert [subject for _, subject in declared] == ["http.v1.info.get"]


def test_the_real_http_component_counts_one_subject_for_its_header():
    project = REPO_ROOT / "projects/ores.http"

    lines = survey.report_protocol(project, project / "modeling")

    assert _header_row(lines, "http_info_protocol.hpp")[2] == "1"
