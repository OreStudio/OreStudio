"""Tests for build/scripts/expand_catch2_cases.py.

Run::

    python3 -m pytest build/scripts/tests/test_expand_catch2_cases.py

CDash shows what Test.xml says, so these cases are the contract: a suite
whose results account for it becomes one entry per case, and a suite whose
results do not (missing, stale, truncated, or green while the suite failed)
stays as it is, so no failure is hidden.
"""
import importlib.util
import os
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "expand_catch2_cases",
    REPO_ROOT / "build" / "scripts" / "expand_catch2_cases.py")
expand_catch2_cases = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = expand_catch2_cases
SPEC.loader.exec_module(expand_catch2_cases)

START = 1_000_000.0

CATCH2_RESULTS = """<?xml version="1.0" encoding="UTF-8"?>
<Catch2TestRun name="ores.demo.tests" xml-format-version="3">
  <TestCase name="adds_two_numbers" tags="[a]" filename="a.cpp" line="10">
    <OverallResult success="true" skips="0" durationInSeconds="0.25"/>
  </TestCase>
  <TestCase name="refuses_a_bad_code" tags="[a]" filename="a.cpp" line="20">
    <Section name="when the code is empty" filename="a.cpp" line="22">
      <Expression success="false" type="CHECK" filename="a.cpp" line="24">
        <Original>result.has_value()</Original>
        <Expanded>false</Expanded>
      </Expression>
    </Section>
    <OverallResult success="false" skips="0" durationInSeconds="1.5"/>
  </TestCase>
  <TestCase name="waits_for_a_feature" tags="[a]" filename="a.cpp" line="30">
    <OverallResult success="true" skips="1" durationInSeconds="0"/>
  </TestCase>
</Catch2TestRun>
"""

ALL_PASS = """<?xml version="1.0" encoding="UTF-8"?>
<Catch2TestRun name="ores.demo.tests" xml-format-version="3">
  <TestCase name="adds_two_numbers" tags="[a]" filename="a.cpp" line="10">
    <OverallResult success="true" skips="0" durationInSeconds="0.25"/>
  </TestCase>
</Catch2TestRun>
"""


def suite(name: str, status: str, results: Path | None, label: str = "demo") -> str:
    out = f' "-r" "xml::out={results}"' if results else ""
    command = f'/bin/{name} "--durations" "yes"{out}'
    return f"""
    <Test Status="{status}">
      <Name>{name}</Name>
      <Path>.</Path>
      <FullName>./{name}</FullName>
      <FullCommandLine>{command}</FullCommandLine>
      <StartTestTime>{START}</StartTestTime>
      <Results>
        <NamedMeasurement type="numeric/double" name="Execution Time"><Value>2</Value></NamedMeasurement>
        <Measurement><Value>suite output</Value></Measurement>
      </Results>
      <Labels><Label>{label}</Label></Labels>
    </Test>"""


def ctest_xml(*suites: tuple, subprojects: tuple[str, ...] = ()) -> ET.ElementTree:
    listed = "".join(f"<Test>./{s[0]}</Test>" for s in suites)
    body = "".join(suite(*s) for s in suites)
    declared = "".join(f'<Subproject name="{n}"><Label>{n}</Label></Subproject>'
                       for n in subprojects)
    text = (f'<Site Name="x" BuildStamp="s">{declared}<Testing>'
            f"<StartTestTime>{START}</StartTestTime><TestList>{listed}</TestList>"
            f"{body}<EndTestTime>0</EndTestTime></Testing></Site>")
    return ET.ElementTree(ET.fromstring(text))


def results_file(tmp_path: Path, text: str, mtime: float = START + 5) -> Path:
    path = tmp_path / "test-results-ores.demo.tests.xml"
    path.write_text(text)
    os.utime(path, (mtime, mtime))
    return path


def entries(tree: ET.ElementTree) -> dict[str, ET.Element]:
    return {t.findtext("Name"): t for t in tree.getroot().iterfind("Testing/Test")}


def test_a_suite_becomes_one_entry_per_case(tmp_path):
    tree = ctest_xml(("ores.demo.tests", "failed", results_file(tmp_path, CATCH2_RESULTS)))
    assert expand_catch2_cases.expand(tree) == (1, 3)
    found = entries(tree)
    assert {n: t.get("Status") for n, t in found.items()} == {
        "ores.demo.tests.adds_two_numbers": "passed",
        "ores.demo.tests.refuses_a_bad_code": "failed",
        "ores.demo.tests.waits_for_a_feature": "notrun",
    }
    listed = [t.text for t in tree.getroot().iterfind("Testing/TestList/Test")]
    assert listed == list(found)


def test_a_case_carries_its_time_command_and_labels(tmp_path):
    tree = ctest_xml(("ores.demo.tests", "failed", results_file(tmp_path, CATCH2_RESULTS)))
    expand_catch2_cases.expand(tree)
    case = entries(tree)["ores.demo.tests.adds_two_numbers"]
    assert expand_catch2_cases.measurement(case, "Execution Time") == "0.250000"
    assert case.findtext("FullName") == "ores.demo.tests.adds_two_numbers"
    assert case.findtext("FullCommandLine") == 'ores.demo.tests "adds_two_numbers"'
    assert expand_catch2_cases.measurement(case, "Command Line") is None
    assert [label.text for label in case.iterfind("Labels/Label")] == ["demo"]


def test_a_failed_case_names_the_failing_check(tmp_path):
    tree = ctest_xml(("ores.demo.tests", "failed", results_file(tmp_path, CATCH2_RESULTS)))
    expand_catch2_cases.expand(tree)
    case = entries(tree)["ores.demo.tests.refuses_a_bad_code"]
    assert case.findtext("Results/Measurement/Value") == (
        "[when the code is empty] a.cpp:24: CHECK( result.has_value() ) "
        "with expansion: false")
    assert expand_catch2_cases.measurement(case, "Exit Code") == "Failed"


def test_a_failed_suite_whose_cases_all_passed_stays(tmp_path):
    tree = ctest_xml(("ores.demo.tests", "failed", results_file(tmp_path, ALL_PASS)))
    assert expand_catch2_cases.expand(tree) == (0, 0)
    assert list(entries(tree)) == ["ores.demo.tests"]


def test_a_suite_without_its_results_file_stays(tmp_path):
    tree = ctest_xml(("ores.demo.tests", "failed", tmp_path / "missing.xml"))
    assert expand_catch2_cases.expand(tree) == (0, 0)
    assert entries(tree)["ores.demo.tests"].get("Status") == "failed"


def test_results_older_than_the_run_are_ignored(tmp_path):
    stale = results_file(tmp_path, ALL_PASS, mtime=START - 3600)
    tree = ctest_xml(("ores.demo.tests", "passed", stale))
    assert expand_catch2_cases.expand(tree) == (0, 0)


def test_results_cut_short_by_a_crash_are_ignored(tmp_path):
    cut = results_file(tmp_path, CATCH2_RESULTS[:400])
    tree = ctest_xml(("ores.demo.tests", "failed", cut))
    assert expand_catch2_cases.expand(tree) == (0, 0)


def test_a_test_that_writes_no_catch2_results_stays(tmp_path):
    tree = ctest_xml(("ores.web.tests", "passed", None),
                    ("ores.demo.tests", "passed", results_file(tmp_path, ALL_PASS)))
    assert expand_catch2_cases.expand(tree) == (1, 1)
    assert list(entries(tree)) == ["ores.web.tests", "ores.demo.tests.adds_two_numbers"]


def test_main_rewrites_the_file_in_place(tmp_path):
    tree = ctest_xml(("ores.demo.tests", "passed", results_file(tmp_path, ALL_PASS)))
    path = tmp_path / "Test.xml"
    tree.write(path, encoding="UTF-8", xml_declaration=True)
    assert expand_catch2_cases.main(["expand_catch2_cases.py", str(path)]) == 0
    assert list(entries(ET.parse(path))) == ["ores.demo.tests.adds_two_numbers"]


def test_split_writes_one_file_per_subproject(tmp_path):
    other = tmp_path / "other"
    other.mkdir()
    tree = ctest_xml(
        ("ores.demo.tests", "failed", results_file(tmp_path, CATCH2_RESULTS)),
        ("ores.other.tests", "passed", results_file(other, ALL_PASS), "other"),
        ("ores.loose.tests", "passed", None, "loose"),
        subprojects=("demo", "other"))
    expand_catch2_cases.expand(tree)
    files = expand_catch2_cases.split(tree, tmp_path / "split")
    assert [f.name for f in files] == ["Test-demo.xml", "Test-other.xml",
                                       "Test-unassigned.xml"]
    assert (tmp_path / "split" / "files.txt").read_text().split() == [str(f) for f in files]

    def read(name):
        root = ET.parse(tmp_path / "split" / name).getroot()
        tests = [t.findtext("Name") for t in root.iterfind("Testing/Test")]
        listed = [t.text for t in root.iterfind("Testing/TestList/Test")]
        declared = [sp.get("name") for sp in root.iterfind("Subproject")]
        return root, tests, listed, declared

    root, tests, listed, declared = read("Test-demo.xml")
    assert root.attrib == {"Name": "x", "BuildStamp": "s"}
    assert declared == ["demo"]
    assert len(tests) == 3 and listed == tests
    assert root.findtext("Testing/StartTestTime") == str(START)
    assert root.findtext("Testing/EndTestTime") == "0"

    _, tests, _, declared = read("Test-other.xml")
    assert (tests, declared) == (["ores.other.tests.adds_two_numbers"], ["other"])

    _, tests, _, declared = read("Test-unassigned.xml")
    assert (tests, declared) == (["ores.loose.tests"], [])


def test_main_splits_when_asked(tmp_path):
    tree = ctest_xml(("ores.demo.tests", "passed", results_file(tmp_path, ALL_PASS)),
                     subprojects=("demo",))
    path = tmp_path / "Test.xml"
    tree.write(path, encoding="UTF-8", xml_declaration=True)
    out = tmp_path / "split"
    assert expand_catch2_cases.main(["x", str(path), "--split", str(out)]) == 0
    assert (out / "files.txt").read_text() == f"{out / 'Test-demo.xml'}\n"
