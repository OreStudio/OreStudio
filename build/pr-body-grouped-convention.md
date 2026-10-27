## Summary

The tenor basis swap convention was carried as three flat structs — a central row of thirteen members and two leg forms of nineteen and eighteen — so no reader could hold one, and each built a wide reflected literal for MSVC to instantiate. This branch groups every one of them onto field groups, and teaches the generated family service to build and read a grouped member.

It is the first application of the ORE parameter contract's element-type rule to a convention: the kind is the element type, its terms name leaves, and the row is a member list rather than a block of columns.

## Changes

- **Group the convention family.** The central `convention` row becomes identity, payload and the `ores.dq.audit_record` tail. Each part becomes the identity it shares with its sibling, one group per leg, its coupon treatment and the same tail. Every group holds two to five members.
- **Teach the family service grouped members.** It refused a grouped central row or part because it builds a domain object from the flat write record and names the key, version and party on both sides of an assignment. The family view now resolves a member's domain groups and gives each column the prefix of the member that carries it; the party, version and commentary are named by path.
- **Prefix four templates** that still wrote a flat path: the generator's natural keys, the repository mapper in both directions, the history field mapper, and the table display.
- **Stamp into a grouped row's members.** `stamp_document()` only stamped a row carrying `modified_by` on the struct, so a grouped row's identity kept a nil party and the first database run refused every write. Each member of an aggregate is now stamped as well as walked into.
- **`ores.dq.api` is public on `ores.refdata.api`**, declared in the component's `component_overview.org` because the generated `CMakeLists.txt` overwrites a hand edit — the generated headers include the audit record.
- **No schema change.** The tables, the columns and the seventeen `TENOR_BASIS_SWAP` kind terms are untouched. Write records and the wire shape stay flat; only the domain struct nests.

## Testing plan

Run the codegen suite, the component drift sweep, and the database suites the change reaches: refdata core (bundle service, bundle import commit, convention eventing) and the ORE conventions round trip.

## Testing evidence

| Gate | Result |
|---|---|
| `pytest projects/ores.codegen/tests` | 1624 passed |
| `check_component_drift.py --sweep --dry-run` | clean |
| `ores.refdata.core.tests` | _(to fill)_ |
| `ores.ore.core.tests` | _(to fill)_ |
| Build | refdata api/core/service/tests, ores.ore core, shell refdata |

## Testing limitations

The Windows MSVC build that the story's translation-unit measurement is about has not been run; the reduction in reflected-literal width is shown by the member counts, not by a passing MSVC build. The web BFF reads conventions through the family protocol and its generated twins were regenerated, but no browser journey was exercised.
