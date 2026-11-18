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
two write different tenants.* A tenant's own row is not a second copy of the
platform's row, and the exception list states which case each remaining table
is. The list shrinks as the task that collapses the writers proceeds; a new
entry needs a reason.

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
    # The world catalogue. The populate scripts below still write the system
    # tenant's live rows, and the world dataset publishes the same rows into
    # it, which is the second copy this check exists to find. They are listed
    # so the check reports no new one while the collapse proceeds; each entry
    # leaves the list as its populate script stops inserting.
    "ores_refdata_calendars_tbl": "to collapse: refdata.calendars publishes it",
    "ores_refdata_calendar_types_tbl": "to collapse: refdata.calendar_types publishes it",
    "ores_refdata_calendar_events_tbl": "to collapse: refdata.calendar_events publishes it",
    "ores_refdata_calendar_exceptions_tbl": "to collapse: refdata.calendar_exceptions publishes it",
    "ores_refdata_currency_calendars_tbl": "to collapse: refdata.currency_calendars publishes it",
    "ores_refdata_currency_countries_tbl": "to collapse: refdata.currency_countries publishes it",
    "ores_refdata_currency_pair_convention_calendars_tbl":
        "to collapse: refdata.currency_pair_convention_calendars publishes it",
    "ores_refdata_tenor_schedules_tbl": "to collapse: refdata.tenor_schedules publishes it",
    "ores_refdata_countries_tbl": "to collapse: iso.countries publishes it",
    "ores_refdata_business_centres_tbl":
        "to collapse: the WRLD row is seeded directly and fpml.business_center publishes the rest",
    "ores_refdata_payment_frequencies_tbl":
        "to collapse: no dataset carries it, so its publish function is dead",
    "ores_refdata_tenor_convention_resolutions_tbl":
        "to collapse: refdata.tenor_schedules publishes it",
    # Outside the refdata inventory the same task took. Both writers reach the
    # system tenant, and the rows agree because the publish finds the series
    # the seed wrote; the derived-feed work owns the collapse.
    "ores_marketdata_market_series_tbl":
        "to collapse: the seeds write the system tenant's bootstrapped series "
        "and the market data publish writes a series for its target tenant",
    "ores_marketdata_market_series_asset_classes_tbl":
        "to collapse: the seeds write the system tenant's series classes and "
        "the market data publish writes them for its target tenant",
    "ores_iam_account_roles_tbl":
        "to collapse: the ACME cut-take seeds grant roles and the IAM publish "
        "grants them for its target tenant",
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
