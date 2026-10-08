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
# dq joins here at the end of its clean-standard pass. Its regeneration is
# byte-identical at all eight addresses, and every one of its protocol headers
# now has a TypeScript twin. The five that lacked one were settled rather than
# suppressed: the badge mapping junction declared no :list_by:, so the junction
# gate dropped its whole messaging stack while a stale twin sat in the tree; the
# publish-from-dq payloads, the LEI entity summary and the report-definition
# template are operation models now; and the dataset-dependency surface, which
# nothing called, is retired. Listing dq is what makes those gates permanent for
# the component.
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
#
# marketdata left the list on 2026-10-04. Its joining record passed items it
# did not pass, and a re-measurement at ec37b05f77 found eleven that fail: W02,
# P01, P02, G02, G04, H01, H03, S01, S02, V04 and V08. The owner chose to take
# it out until every item passes rather than accept the eleven, so the drift and
# protocol twin gates do not cover it until it returns. The measurement and the
# task that removes each failure are on
# doc/agile/versions/v0/sprint_26/oresmd-handwritten-grammar/task_correct-the-clean-standard-record.org.
#
# marketdata rejoins here at the end of its return task. Every one of the eleven
# failures is closed by the task that owned it: W02 by the permission gate,
# P02, G02 and H03 by the code clean, S01, S02 and V04 by the shell commands,
# G04 and V08 by the test coverage, and P01 by the generated point-in-time list
# read. H01 is met for the four part diagrams, which are authored in two passes;
# the composite root carries a hand-authored component diagram, whose reason is
# recorded on
# doc/agile/versions/v0/sprint_27/return-marketdata-to-the-clean-standard-registry/task_return-marketdata-to-the-clean-standard-registry.org.
# The whole record is re-measured at one commit there.
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
    "marketdata": (
        AcceptedException(
            item="H01",
            reason=(
                "The four part diagrams are authored in two passes: the "
                "automated pass refreshed every box and the manual section "
                "below each sentinel carries the derived edges and the notes "
                "for what the parser cannot read, with each rendered image "
                "read. The composite root is the one exception: it owns no "
                "code, so the automated pass, which reads C++ headers, has "
                "nothing to contribute to it, and the root carries a "
                "hand-authored component diagram of the four parts and their "
                "measured dependencies, with the oresmd data-model diagram "
                "kept beside it. This is the position every other listed "
                "composite takes."
            ),
            accepted_by="marco",
            accepted_on="2026-10-06",
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
# synthetic joins at the end of its clean-standard pass. It had generated
# C++ that no address regenerated, so 90 committed files had drifted from
# the templates -- protocol headers and includes -- while the gate reported
# a clean tree. Its regeneration is byte-identical across every address now,
# and its CMake source lists are current.
#
# scheduler-cpp joins at the end of its clean-standard pass. It had
# generated C++ that no address regenerated, so its committed output had
# drifted from the templates while the gate reported a clean tree. Its
# regeneration is byte-identical across every address now, and its CMake
# source lists are current.
#
# inbox-cpp joins at the end of its clean-standard pass. It had generated
# C++ that no address regenerated, so 95 committed files had drifted from
# the templates -- include lists across its core registrars -- while the
# gate reported a clean tree. Its regeneration is byte-identical across
# every address now, and its CMake source lists are current.
#
# shell joins at the end of its clean-standard pass. It is a component of kind
# All with no entity, junction or operation model: its one model is the
# component model, and the shell units in its tree are output of the other
# components' models. Its regeneration is byte-identical at all six addresses,
# its two CMake source lists are current, it carries one namespace
# documentation header for its outermost namespace, and every item that does
# not apply to a component of kind All is recorded with its reason on
# doc/agile/versions/v0/sprint_26/clean-shell/task_clean_shell.org.
COMPONENTS_UNDER_TEST = ("iam", "analytics-cpp", "assets-cpp", "compute-cpp", "dq", "http-cpp", "inbox-cpp", "marketdata", "ore", "refdata", "reporting", "scheduler-cpp", "shell", "synthetic", "telemetry-cpp", "trading-cpp", "variability-cpp", "workflow-cpp")
