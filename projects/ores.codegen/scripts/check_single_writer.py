#!/usr/bin/env python3
"""Check that no table is written both by a populate script and by a DQ
publish function, unless it is a reviewed exception.

The reference data model gives every table one writer in a tenant, and the
level the data belongs to chooses it: a fixed vocabulary the platform ships is
written by a populate script, and a catalogue a person manages is published as
a dataset. Two writers of one table in one tenant produce two copies of a fact,
and no reader is then authoritative -- which is how the calendars the platform
seeded came to differ from the calendars the dataset published.

The check reads the tree and needs no database. A *publish function* is one a
dataset reaches through its artefact type, so the scan takes the tables an
``insert into`` names inside any ``*_publish_from_dq_create.sql`` file, and
intersects them with the tables any ``populate/**/*.sql`` file inserts into.

*A populate script may write a table that a publish function writes when the
two do not write the same row.* Two cases qualify. The two may write different
tenants: a tenant's own row is not a second copy of the platform's row. Or they
may write disjoint rows of one tenant: the platform defines one code and the
dataset adopts the rest. The exception list states which case each remaining
table is, and names the rows it covers. The list shrinks as the task that
collapses the writers proceeds; a new entry needs a reason.

Run::

    python3 projects/ores.codegen/scripts/check_single_writer.py
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SQL_DIR = REPO_ROOT / "projects" / "ores.sql"
PUBLISH_DIR = SQL_DIR / "create"
POPULATE_DIR = SQL_DIR / "populate"

PUBLISH_FILE = re.compile(r"_publish_from_dq_create\.sql$")

INSERT_RE = re.compile(r"insert\s+into\s+\"?(\w+)\"?", re.I)

# The system tenant's copy and a tenant's own copy are two tenants, and each
# holds one copy. These are the tables where that is the whole of the overlap.
TENANTS = "the populate script writes the system tenant's row and the publish writes a tenant's own"

EXCEPTIONS = {
    "ores_refdata_parties_tbl":
        f"{TENANTS}: the system party, and each tenant's own parties",
    "ores_refdata_business_unit_types_tbl":
        f"{TENANTS}: the platform's five types, and a tenant's own five",
    "ores_assets_images_tbl":
        f"{TENANTS}: a demonstration's photographs in its own tenant, and the "
        "platform's flags and avatars in the system tenant",
    "ores_marketdata_market_series_tbl":
        f"{TENANTS}: the seeds write the system tenant's bootstrapped series, "
        "and the market data publish writes a series for the tenant that asked "
        "for it",
    "ores_marketdata_market_series_asset_classes_tbl":
        f"{TENANTS}: the seeds write the system tenant's series classes, and "
        "the market data publish writes them for the tenant that asked for it",
    # Two tables where the two writers write disjoint rows of one tenant rather
    # than a copy of one fact. The rows are named, because the overlap is a
    # property of the row and not of the table.
    "ores_refdata_business_centres_tbl":
        "the platform's WRLD row and the 212 FpML codes are disjoint rows: the "
        "populate script writes WRLD, because the foundation layer's system "
        "party names it and no dataset can publish that early, and "
        "fpml.business_center publishes the rest",
    "ores_refdata_payment_frequencies_tbl":
        "no dataset carries it, so the generated publish function is "
        "unreachable and the populate script is the platform vocabulary's one "
        "writer",
    "ores_refdata_tenor_convention_resolutions_tbl":
        "the row is the platform's -- which tenors a convention carries, and "
        "how each resolves -- and two of its columns are the world dataset's: "
        "refdata.tenor_schedules re-inserts each row to set schedule_code and "
        "schedule_step_count, so it writes a new version of a row the populate "
        "script wrote rather than a row of its own",
    # The one table that still holds a row written twice, and what it needs.
    "ores_iam_account_roles_tbl":
        "to collapse: the ACME cut-take seed grants a second role to one "
        "account, which the account artefact cannot express because it carries "
        "one role per account; the IAM publish grants that one",
}


def strip_comments(sql):
    sql = re.sub(r"/\*.*?\*/", "", sql, flags=re.S)
    return re.sub(r"--[^\n]*", "", sql)


def inserted(sql):
    """The lowercase table names an SQL text inserts into."""
    return {m.group(1).lower() for m in INSERT_RE.finditer(strip_comments(sql))}


def published(publish_dir=PUBLISH_DIR):
    """The tables a publish function inserts into."""
    tables = set()
    for path in sorted(publish_dir.rglob("*_publish_from_dq_create.sql")):
        if PUBLISH_FILE.search(path.name):
            tables |= inserted(path.read_text(encoding="utf-8"))
    return tables


def seeded(populate_dir=POPULATE_DIR):
    """table -> the populate files that insert into it."""
    found = {}
    for path in sorted(populate_dir.rglob("*.sql")):
        for table in inserted(path.read_text(encoding="utf-8")):
            found.setdefault(table, []).append(path.relative_to(REPO_ROOT))
    return found


def writers(populate_dir=POPULATE_DIR, publish_dir=PUBLISH_DIR):
    """Every (table, files) written by a populate script and a publish."""
    published_tables = published(publish_dir)
    return {table: files for table, files in seeded(populate_dir).items()
            if table in published_tables}


def main():
    found = writers()
    unlisted = {t: f for t, f in found.items() if t not in EXCEPTIONS}
    stale = sorted(set(EXCEPTIONS) - set(found))
    for table, files in sorted(unlisted.items()):
        for path in files:
            print(f"error: {path}: writes {table}, which a DQ publish function "
                  f"also writes. One table in one tenant has one writer: keep "
                  f"whichever the level assigns, and remove the other. A "
                  f"populate script that writes a different tenant is not a "
                  f"second copy -- list it with its reason.")
    for table in stale:
        print(f"error: exception {table} names no table written both ways; "
              f"remove it")
    if unlisted or stale:
        return 1
    print(f"ok: {len(found)} table(s) written both ways, all listed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
