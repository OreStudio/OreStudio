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

from dataclasses import dataclass

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
# dq is the next component to join and is deliberately not listed yet. Its
# regeneration is clean -- check_component_drift.py --component dq reports no
# drift -- but it cannot pass this list's twin claim: twelve of its protocol
# headers are hand-written for entities that have no model yet (datasets,
# coding schemes, publications, the FSM family, dimensions, methodologies,
# report-definition templates, and the badge mapping projection), so they have
# no TypeScript twin and none can be generated. Listing dq would fail
# check_protocol_twin_coverage.py on those twelve. It joins once the remaining
# entities are modelled.
#
# The ores.assets clean-standard task adds assets-cpp on the same terms: the
# component regenerates byte for byte, every codegen gate passes with it
# listed, and the whole tree builds with it.
#
# compute-cpp joins it for the clean-compute task. Its regeneration leaves the
# tree clean at every address, every protocol header has its TypeScript twin,
# and its seeds are complete. The hand-written files that remain are the
# component's infrastructure, and the task records why generation does not
# supersede each:
# doc/agile/versions/v0/sprint_26/clean-compute/task_clean_compute.org.
#
# analytics-cpp joins here: its models are bound and on the current format,
# its regeneration is byte-identical, and its shell, SQL and TypeScript
# derivatives are committed. Joining is the last step of its clean-standard
# story, so the registry is the record of which components are clean.
#
# ore joins it at the end of its clean-standard pass. The same three claims
# hold: regeneration is byte-identical across every facet, every one of its
# protocol headers has a TypeScript twin, and its seeds are complete.
# Registering it is what makes the gates permanent for the component rather
# than a check somebody remembers to run. ores.marketdata and every other
# component that is not yet regenerable stays out for the reason refdata does:
# an un-regenerated component's whole backlog rides along with any model
# change to it.
#
# synthetic is not listed yet: its regeneration is byte-identical and
# idempotent and all four of its test suites pass, but five checklist items are
# open (B06 and V08, the two surveys; M06, no TypeScript twin; P03, the
# messaging reference is stale; V04, no live fleet). The standard keeps a
# component out until every item passes or its exceptions are accepted, so add
# it once they are.

# workflow joins on the same terms at the end of its clean-standard pass: its
# tree was regenerated from its two entity models, the drift check is
# byte-identical and idempotent, and the component builds. Its protocol headers
# carry no TypeScript twin yet, so the twin gate is the one claim to check
# before this line is merged.
# telemetry-cpp joins at the end of its clean-standard pass. It was the first
# Protocol-kind component through the standard: its three models are operation
# models for the six subjects it serves and sends, and the repository reads
# the payload types those models generate. Its regeneration is byte-identical
# and idempotent, its three suites pass, and every item is recorded as a pass
# or as not applicable. Two of its own tests were deleted rather than
# strengthened because the id generator takes the system clock, and four stats
# reads are kept with a capture naming the consumer that should reach them.
# variability joins it at the end of its clean-standard pass. Its regeneration is
# byte-identical on a committed tree, every one of its protocol headers has a
# TypeScript twin, the database recreates from scratch with its generated table,
# triggers and policies, the eleven seeded settings land, and its four test
# suites pass with the fleet's NATS up. What remains hand-written is recorded
# with its reason on the task:
# doc/agile/versions/v0/sprint_26/clean-variability/task_clean_variability.org.
@dataclass(frozen=True)
class AcceptedException:
    """One checklist item a listed component does not pass.

    Listing a component is a claim that every checklist item that applies to it
    passes. Sometimes an item cannot be made to pass in the environment the work
    was done in -- a check that needs a live fleet, a tool that cannot draw what
    the standard asks for -- and the honest thing is to run every gate that
    *can* run rather than withhold the component from all of them.

    That is what this records: the component is listed, the gates check it, and
    the item that does not pass is named here with its reason and the person who
    accepted it. An item is never simply omitted -- a component with an item
    that is neither passing nor recorded here cannot be listed, which
    ``check_registry_exceptions.py`` enforces against the standard's own item
    list. The reason is not free text to be skimmed: it is the thing a reviewer
    reads to decide whether the acceptance still holds.
    """

    item: str
    reason: str
    accepted_by: str
    accepted_on: str


# The accepted exceptions, by component. A component absent from this mapping
# passes every item that applies to it.
#
# variability-cpp held an H01 exception here, on the grounds that the diagram
# capture read data members and not methods, so a class whose content is
# methods arrived without its API. The capture now reads member functions, so
# the exception was retired and the component's diagrams regenerated from it.
#
# iam was listed from its clean-standard pass while four items did not pass and
# no exception was recorded, so the listing claimed more than the task record
# did. The four are named here now, each with the task row that carries it, and
# the security defects the same pass recorded are closed.
ACCEPTED_EXCEPTIONS: dict[str, tuple[AcceptedException, ...]] = {
    "variability-cpp": (
        AcceptedException(
            item="V08",
            reason=(
                "The in-process facade added by this work, "
                "system_settings_service, has no unit test of its own. Its "
                "behaviour is covered end to end by the iam and http suites and "
                "by the generated eventing integration test, but not directly."
            ),
            accepted_by="marco",
            accepted_on="2026-09-26",
        ),
    ),
    "iam": (
        AcceptedException(
            item="P01",
            reason=(
                "Two protocol decisions are recorded rather than settled. The "
                "account entity is :read_only:, so its writes are bespoke "
                "operation verbs with success/message envelopes instead of the "
                "canonical put and delete; and account_contact_information "
                "declares :list_by_as_of:, which generates a point-in-time "
                "read no caller uses and whose as_of never reaches the wire. "
                "Both are design calls for the owner, not cleanup."
            ),
            accepted_by="marco",
            accepted_on="2026-09-27",
        ),
        AcceptedException(
            item="P04",
            reason=(
                "The item is met: the nine non-entity surfaces are operation "
                "models and generate their messages, with no hand-written "
                "protocol header left. Two estate-wide conventions differ and "
                "are recorded rather than changed here: the operation subjects "
                "use no .v1.ops. namespace, and operation responses carry "
                "success/message where the entity CRUD responses carry "
                "ores::utility::domain::result. Both are house decisions."
            ),
            accepted_by="marco",
            accepted_on="2026-09-27",
        ),
        AcceptedException(
            item="S01",
            reason=(
                "Three hand-written units extend generated menus rather than "
                "registering menus of their own: accounts carries twelve verbs "
                "(create, login, lock, unlock, list-logins, logout, sessions, "
                "sessions-for, active-sessions, history, info and "
                "set-default-party), permissions carries suggest, and tenants "
                "carries history and complete-provisioning. The root-level "
                "login and logout aliases are the same kind of remnant. None of "
                "these can be shadowed any more: the shell's root refuses a "
                "second claim on a name at startup, which is what the menu "
                "collision story changed, so the overlapping-registration half "
                "of this exception no longer describes anything. What remains "
                "open is only which of these verbs the generation supersedes, "
                "and that is a per-verb call-site question: login has no "
                "generated unit, so the wholesale deletion the item asks for "
                "would remove live behaviour. The mapping is on the task."
            ),
            accepted_by="marco",
            accepted_on="2026-09-27",
        ),
        AcceptedException(
            item="V08",
            reason=(
                "The per-file survey is complete and recorded: 87 of 151 "
                "source files are touched by a test and 64 are not, each with "
                "its directory and its reason. The untouched list is the "
                "coverage work item the item names, not a hole in the pass."
            ),
            accepted_by="marco",
            accepted_on="2026-09-27",
        ),
    ),
}


def accepted_exceptions(component: str) -> tuple[AcceptedException, ...]:
    """The items accepted against one component; empty when it has none."""
    return ACCEPTED_EXCEPTIONS.get(component, ())


# http-cpp joins at the end of its clean-standard pass. It is a Protocol
# component with no entity of its own: its one wire type is an operation
# model, whose C++ header and TypeScript twin both generate from it, and
# codegen owns its composite root CMakeLists.txt. Its regeneration is
# byte-identical across every address, and its CMake source lists are
# current.
#
# shell joins at the end of its clean-standard pass. It is a component of kind
# All with no entity, junction or operation model: its one model is the
# component model, and the shell units in its tree are output of the other
# components' models. Its regeneration is byte-identical at all six addresses,
# its two CMake source lists are current, it carries one namespace
# documentation header for its outermost namespace, and every item that does
# not apply to a component of kind All is recorded with its reason on
# doc/agile/versions/v0/sprint_26/clean-shell/task_clean_shell.org.
COMPONENTS_UNDER_TEST = ("iam", "analytics-cpp", "assets-cpp", "compute-cpp", "http-cpp", "ore", "reporting", "shell", "telemetry-cpp", "workflow-cpp", "variability-cpp")
