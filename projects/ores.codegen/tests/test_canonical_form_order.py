"""Every generated canonical form renders its members in the struct's order.

Run with:
    python3 -m pytest projects/ores.codegen/tests/test_canonical_form_order.py

The economic digest hashes the canonical form, so the order of its members is
part of every stored digest. The form is generated from the model, and the
struct's order is stated by its wire_fields. This test reads both from the
checked-in headers and requires the form's members to be a subsequence of the
shape's, so a generator change that reorders a form fails here and not in a
stored digest.
"""
import re
import subprocess
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]

FORM = re.compile(r"canonical_form\(const (\w+)&(?: v)?\) \{(.*?)\n\}", re.S)
MEMBER = re.compile(r'append_member\(out, "(\w+)"')


def _forms():
    files = subprocess.run(
        ["git", "grep", "-l", "inline std::string canonical_form(const", "--", "projects"],
        cwd=REPO_ROOT, capture_output=True, text=True, check=True).stdout.split()
    for name in files:
        path = REPO_ROOT / name
        text = path.read_text(encoding="utf-8")
        codec = path.with_name(path.stem + "_codec.hpp")
        shapes = text + (codec.read_text(encoding="utf-8") if codec.is_file() else "")
        for form in FORM.finditer(text):
            yield name, form.group(1), MEMBER.findall(form.group(2)), shapes


def test_the_forms_are_found():
    assert len(list(_forms())) > 50


def test_every_form_follows_the_declared_member_order():
    wrong = []
    for name, owner, members, shapes in _forms():
        shape = re.search(
            r"wire_fields\(std::type_identity<%s>\)\s*\{(.*?)\n\}" % owner, shapes, re.S)
        if shape is None:
            wrong.append(f"{name}: {owner} states no shape")
            continue
        order = iter(re.findall(r'field\{"(\w+)"', shape.group(1)))
        if not all(member in order for member in members):
            wrong.append(f"{name}: {owner} renders {members} out of order")
    assert not wrong, "\n".join(wrong)
