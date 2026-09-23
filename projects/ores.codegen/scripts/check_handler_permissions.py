#!/usr/bin/env python3
"""Check that every permission a generated NATS handler checks is seeded.

A generated handler refuses a caller when ``has_permission(req_ctx, "<code>")``
is false, and the check resolves the code against the rows
``iam_permissions_populate.sql`` writes. A code no seed defines therefore
denies every caller, administrator roles included, which reads as a broken
feature rather than as a missing grant.

The codes come from the handler files themselves, generated and
hand-written alike, so a resource joins the check the moment its handler
lands -- nothing here lists the resources. Only the components in
``REGISTRY`` are checked, because a component whose seeds are still
incomplete would fail the tree for a gap that predates this check. A
component joins the registry once its seeds are complete, which is the same
rollout rule the drift gate uses.

Run::

    python3 projects/ores.codegen/scripts/check_handler_permissions.py
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
POPULATE = (
    REPO_ROOT / "projects" / "ores.sql" / "populate" / "iam"
    / "iam_permissions_populate.sql"
)

# Components whose handler permission codes are all seeded. A component joins
# this registry once its seeds are complete; until then its gap stays a known
# one rather than a surprise in a gate. This list is the bare project name,
# because a component's handlers are found under projects/ores.<name>, and it
# is deliberately separate from the shared component_registry list, which
# carries catalogue slugs. analytics joined with its clean-standard pass, whose
# W02 item this check proves; compute and assets joined with theirs.
REGISTRY = ("refdata", "analytics", "compute", "assets")

# has_permission takes the request context first, so the code is the string
# literal argument. A helper that passes the code through a variable is not
# visible here, which the "not vacuous" test below guards against.
PERMISSION_RE = re.compile(r'has_permission\(\s*[^,]+,\s*"([^"]+)"')

# The seed scripts pass each code as a string literal to the upsert function.
SEED_RE = re.compile(r"'(?P<code>[a-z_]+::[a-z_]+:[a-z_-]+)'")


def handler_headers(component: str) -> list[Path]:
    """Every NATS handler file the component carries.

    Both extensions: codegen writes its handlers as headers, but a
    component also carries hand-written handlers whose methods check
    permissions. A glob over ``*_handler.hpp`` alone would leave the
    hand-written ones invisible, so a code only they check could stay
    unseeded with the gate green.
    """
    root = REPO_ROOT / "projects" / f"ores.{component}"
    return sorted(
        list(root.rglob("messaging/*_handler.hpp"))
        + list(root.rglob("messaging/*_handler.cpp")))


def required_codes(component: str) -> dict[str, set[str]]:
    """``{header path relative to the repo: {permission code, ...}}``."""
    out: dict[str, set[str]] = {}
    for path in handler_headers(component):
        codes = set(PERMISSION_RE.findall(path.read_text(encoding="utf-8")))
        if codes:
            out[str(path.relative_to(REPO_ROOT))] = codes
    return out


def seeded_codes(path: Path = POPULATE) -> set[str]:
    """Every permission code the populate scripts define."""
    return set(SEED_RE.findall(path.read_text(encoding="utf-8")))


def check(registry: tuple[str, ...] = REGISTRY,
          seed_path: Path = POPULATE) -> list[tuple[str, str]]:
    """Every code the registry's handlers require but no seed defines."""
    seeded = seeded_codes(seed_path)
    violations: list[tuple[str, str]] = []
    for component in registry:
        for path, codes in sorted(required_codes(component).items()):
            for code in sorted(codes - seeded):
                violations.append((path, code))
    return violations


def main() -> int:
    violations = check()
    for path, code in violations:
        print(f"{path}: {code} is checked but never seeded")
    if violations:
        print(f"\n{len(violations)} unseeded handler permission(s).")
        return 1
    # Distinct codes, not (file, code) pairs: two handlers may check the
    # same code -- a resource's save and delete methods share its :write.
    codes = set().union(*(
        found for component in REGISTRY
        for found in required_codes(component).values()))
    print(f"handler permissions are seeded ({len(REGISTRY)} component(s), "
          f"{len(codes)} distinct code(s)).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
