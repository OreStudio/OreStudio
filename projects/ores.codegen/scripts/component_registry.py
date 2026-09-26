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
# Listing a component claims responsibility for regenerating it. The branch
# changes shared codegen -- the service key signatures, the generated-file
# marker, the domain equality -- so every component's checked-in output is
# affected at once. refdata is deliberately not listed: its adoption is in
# flight on its own branch, and listing it here pulled 254 of its files plus
# its shell, sql and web derivatives into this branch's diff. main had it
# listed; that belongs to the refdata work, not here.
#
# compute-cpp joins it for the clean-compute task. Its regeneration leaves the
# tree clean at every address, every protocol header has its TypeScript twin,
# and its seeds are complete. The hand-written files that remain are the
# component's infrastructure, and the task records why generation does not
# supersede each:
# doc/agile/versions/v0/sprint_26/clean-compute/task_clean_compute.org.
COMPONENTS_UNDER_TEST = ("iam", "compute-cpp")
