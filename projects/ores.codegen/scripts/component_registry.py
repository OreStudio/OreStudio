#!/usr/bin/env python3
"""The one list of components the codegen gates verify.

Every component in the catalogue starts as to-do: its models, generated
C++, SQL, TypeScript and seeds are still being brought over, so a gate
that enforced them would fail on work no task has reached yet. A component
joins this list once its whole rollout is finished and verified, and from
then on every gate checks it. The gates import this one list rather than
keeping lists of their own, so two of them cannot disagree about which
components are done.

The list is deliberately short. Adding a component is a claim that its
regeneration leaves the tree clean, that every protocol header has its
TypeScript twin, and that its seeds are complete -- whichever gates exist
at the time. Everything outside the list is to-do, not exempt.

Usage: imported by the check scripts, never run.
"""

# The components under test. Every other catalogue component is to-do.
#
# iam only, on this branch. The branch changes shared codegen -- the service
# key signatures, the generated-file marker, the domain equality -- so every
# component's checked-in output is affected at once, and listing a component
# here claims responsibility for regenerating it. refdata is deliberately not
# listed: its adoption is in flight on its own branch, and listing it here
# pulled 254 of its files plus its shell, sql and web derivatives into this
# branch's diff. main had it listed; that belongs to the refdata work, not
# here.
COMPONENTS_UNDER_TEST = ("iam",)
