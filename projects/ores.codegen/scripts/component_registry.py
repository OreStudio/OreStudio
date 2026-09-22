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
COMPONENTS_UNDER_TEST = ("iam", "refdata")
