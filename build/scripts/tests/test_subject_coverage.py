"""The subject-coverage check: what it reads, how it decides, and that it fails.

The check asks the bus, so these cases inject the asker instead of starting a
fleet. What they hold is the part that decides: which subjects a model makes
the check responsible for, how a CLI answer is read, and that one unanswered
subject is a failure once nothing excuses it.
"""
import importlib.util
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
spec = importlib.util.spec_from_file_location(
    "subject_coverage", ROOT / "build/scripts/check_subject_coverage.py")
coverage = importlib.util.module_from_spec(spec)
spec.loader.exec_module(coverage)


def model(root: Path, component: str, body: str) -> Path:
    folder = root / "projects" / component / "modeling"
    folder.mkdir(parents=True, exist_ok=True)
    path = folder / f"{component}.operations.org"
    path.write_text(body)
    return path


REQUESTED = """\
** get_a_thing_request
:PROPERTIES:
:subject: demo.v1.ops.get_a_thing
:response: get_a_thing_response
:END:

Reads one thing.
"""

PUBLISHED = """\
** tick_message
:PROPERTIES:
:subject: demo.v1.ops.tick
:auth: none
:END:

One live observation.
"""

TEMPLATED = """\
** get_entity_history_request
:PROPERTIES:
:subject: {component}.v1.history.get
:response: get_entity_history_response
:END:

The history of one entity.
"""


def test_the_check_is_responsible_for_requests_and_not_for_publications(tmp_path):
    model(tmp_path, "demo", REQUESTED + PUBLISHED + TEMPLATED)
    subjects, templated, published = coverage.declared_subjects(tmp_path)

    assert [subject for subject, _, _ in subjects] == ["demo.v1.ops.get_a_thing"]
    assert subjects[0][1] == "get_a_thing_request"
    assert subjects[0][2] == "projects/demo/modeling/demo.operations.org"
    assert published == 1
    assert templated and "{component}.v1.history.get" in templated[0]


@pytest.mark.parametrize("stderr,returncode,expected", [
    ('Sending request on "x"\nNo responders are available\n', 0, coverage.ABSENT),
    ('Sending request on "x"\nReceived with rtt 2ms\nX-Error: unauthorized\n', 0,
     coverage.SERVED),
    ('nats: timeout\n', 1, coverage.SILENT),
    ('nats: no servers available for connection\n', 1, coverage.SILENT),
])
def test_an_answer_is_read_from_what_the_cli_said(stderr, returncode, expected):
    assert coverage.classify(stderr, returncode) == expected


def test_a_declared_subject_that_nobody_answers_is_a_finding(tmp_path):
    model(tmp_path, "demo", REQUESTED)
    asked = []

    def answers_nothing(nats, subject, timeout):
        asked.append(subject)
        return coverage.ABSENT

    total, served, findings, _, _ = coverage.check(
        tmp_path, ["nats"], "ores.test", 1, ask_fn=answers_nothing)

    assert (total, served) == (1, 0)
    assert findings == [("demo.v1.ops.get_a_thing", "get_a_thing_request",
                         "projects/demo/modeling/demo.operations.org", coverage.ABSENT)]
    assert asked == ["ores.test.demo.v1.ops.get_a_thing"]


def test_the_same_subject_passes_when_something_answers_it(tmp_path):
    model(tmp_path, "demo", REQUESTED)

    def answers_everything(nats, subject, timeout):
        return coverage.SERVED

    total, served, findings, _, _ = coverage.check(
        tmp_path, ["nats"], "ores.test", 1, ask_fn=answers_everything)

    assert (total, served, findings) == (1, 1, [])
    assert coverage.triage(findings, {}) == ([], [])


def test_an_exception_excuses_its_subject_until_the_subject_answers():
    finding = ("demo.v1.ops.get_a_thing", "get_a_thing_request", "path", coverage.ABSENT)
    exceptions = {"demo.v1.ops.get_a_thing": {"reason": "known", "task": "T"}}

    unexcused, answered = coverage.triage([finding], exceptions)
    assert (unexcused, answered) == ([], [])

    unexcused, answered = coverage.triage([], exceptions)
    assert unexcused == []
    assert answered == ["demo.v1.ops.get_a_thing"]
