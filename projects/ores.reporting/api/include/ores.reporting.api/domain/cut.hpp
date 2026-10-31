/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REPORTING_API_DOMAIN_CUT_HPP
#define ORES_REPORTING_API_DOMAIN_CUT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief A body of data fixed at one timepoint, with the acts that made it.
 *
 * The record of one cut: a body of data fixed at one timepoint, the four inputs
 * the valuation reads, the gaps the assembly could not fill, and the acts that
 * made it. A report's run resolves its selection to a cut and prices from the
 * body the cut names, so a result is reproduced by naming its cut rather than by
 * re-reading a rule.
 *
 * The state a reader sees is a function of these facts and is stored nowhere, as
 * the published set's state is. Draft when no taking instant holds and no gap
 * stands; incomplete when no taking instant holds and a gap stands; taken when the
 * body is fixed, the kind requires an authorisation and none is recorded; in use
 * when the body is fixed and the kind requires none; authorised when an approval
 * is recorded; refused when a refusal is recorded; superseded when a later cut of
 * the same kind and scope holds its place.
 *
 * The label a person quotes is derived from the kind, the scope and the timepoint,
 * and the take ordinal where a body is taken more than once for one occasion:
 * EOD.ACME-LONDON.2026-10-06 and EOD.ACME-LONDON.2026-10-06.2. A live cut
 * carries its instant to the second instead of an ordinal. No column holds the
 * label.
 *
 * The retention is a copy and not a read: a cut takes the kind's retention_months,
 * retention_destination_code and legal_hold_default when it is taken, so a
 * later change to the kind does not move the retention of a body already fixed.
 * Nothing removes a cut today; the archive is a later task.
 */
struct cut final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this cut.
     */
    boost::uuids::uuid id;

    /**
     * @brief The occasion this cut was taken for, from the cut kind lookup.
     *
     * The kind fixes the instant's meaning, the recurrence and the centre, and it declares whether
     * a cut of the occasion needs an authorisation.
     */
    std::string kind_code;

    /**
     * @brief The scope the cut fixes: the book population valued against one centre's market data,
     * named by the code a person quotes in the label.
     *
     * The books themselves are rows of ores.reporting.cut_book.
     */
    std::string scope_code;

    /**
     * @brief The instant the body was fixed at.
     *
     * A body fixed at an instant is not fixed until the instant is stated with its clock, so
     * timepoint_timezone travels with it.
     */
    std::chrono::system_clock::time_point timepoint;

    /**
     * @brief Which take of this occasion the row is: 1 for the first body fixed for the kind, scope
     * and timepoint, 2 for a retake.
     *
     * A retake of the same day, kind and scope is the second cut of that day, and the label states
     * the ordinal so a person reads which body is meant.
     */
    int take_ordinal = 0;

    /**
     * @brief The party whose books and market data the cut fixes.
     *
     * Set from the session that takes the cut, and enforced by row level security, so a party sees
     * only its own cuts.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The rates centre whose market data the scope's books revalue against.
     *
     * A body that names no centre denotes whichever centre answered first, so the centre is part of
     * the scope and not a detail of the assembly.
     */
    std::string business_centre_code;

    /**
     * @brief The timezone the timepoint was stated in, as an IANA name such as Europe/London.
     *
     * Two cuts of the same instant in two centres are two bodies; the clock is what tells a reader
     * which close the instant is.
     */
    std::string timepoint_timezone;

    /**
     * @brief The named cut time the timepoint came from, where a cut time supplied it, or null
     * where the kind's own rule fixed the instant.
     *
     * A soft reference to the cut time reference data, which is a later task; a cut whose instant
     * came from a named time keeps the code so a reader can quote it.
     */
    std::optional<std::string> cut_time_code;

    /**
     * @brief The market data snapshot that resolved every market role at the timepoint, or null
     * where the assembly could not fix one and recorded a gap instead.
     *
     * The snapshot names the published set each role resolved to, so a rerun replays the curves the
     * body priced from rather than the rule that chose them.
     */
    std::optional<boost::uuids::uuid> market_data_snapshot_id;

    /**
     * @brief The copy of the deal population the store held at the timepoint, or null where the
     * assembly could not copy it.
     *
     * A copy and not a query: the interval a versioned row states is rewritten when a correction
     * arrives, so two executions of one query at different moments can return different bodies.
     */
    std::optional<boost::uuids::uuid> deal_population_id;

    /**
     * @brief The version of the reference data in force at the timepoint: the conventions, the
     * calendars, the settings and the curve configurations the run reads.
     */
    std::optional<std::string> reference_data_version;

    /**
     * @brief The version of the risk engine the body was assembled for.
     *
     * A cut that fixed the data and omitted the engine version would hold a body from which its own
     * result does not follow.
     */
    std::optional<std::string> engine_version;

    /**
     * @brief The revision of the code that computed the result.
     */
    std::optional<std::string> code_version;

    /**
     * @brief The account that fixed the body. For a live cut the run writes itself acting for the
     * run's account.
     */
    std::string taken_by;

    /**
     * @brief When the body was fixed, or null when the row is a draft or an incomplete attempt that
     * no take completed.
     *
     * The instant is the take and not the timepoint: a body saved before it is taken carries no
     * taking instant and no cut exists yet.
     */
    std::optional<std::chrono::system_clock::time_point> taken_at;

    /**
     * @brief Whether the kind that took this cut required an authorisation, recorded at the take
     * from the kind's approval kind.
     *
     * The column is what tells a cut that waits on a person from a cut that never needed one: a cut
     * of a kind whose approval kind is null records that none was required rather than looking
     * unsigned.
     */
    bool requires_authorisation = false;

    /**
     * @brief The approval request the take raised, or null where the kind requires none.
     *
     * A soft reference to ores_inbox_approval_requests_tbl: the request is the inbox's record of
     * the wait and the cut links it, so neither copies the other's facts. The take raises the
     * request in the same act that freezes the body.
     */
    std::optional<boost::uuids::uuid> approval_request_id;

    /**
     * @brief The account whose decision authorised the cut, or empty while it waits.
     *
     * The decider is not the taker: the inbox refuses a decision by the person who raised the
     * request, so the two are two people by construction.
     */
    std::string approved_by = "";

    /**
     * @brief When the authorisation was recorded, or null while it waits or where none was
     * required.
     *
     * The reporting service stamps it when the request reaches approved, by reconciliation over the
     * request's state, so a missed, repeated or late event converges on one outcome.
     */
    std::optional<std::chrono::system_clock::time_point> approved_at;

    /**
     * @brief When the authorisation was refused, or null while it waits or where none was required.
     *
     * A refusal is terminal for that body: the inbox allows no second decision on a refused
     * request, and the remedy is a new take, which is a new cut and a new request.
     */
    std::optional<std::chrono::system_clock::time_point> refused_at;

    /**
     * @brief Why the authorisation was refused, in the decider's words, or empty.
     *
     * Copied from the request so the cut's own page reads the reason without a second read, and
     * never the reverse.
     */
    std::string refusal_comment = "";

    /**
     * @brief How long this cut is kept, copied from its kind at the take.
     *
     * Retention is a floor: a person may extend it, and never shorten it below the kind's period.
     */
    int retention_months = 0;

    /**
     * @brief Where this cut goes when it leaves the warm tier, copied from its kind at the take.
     */
    std::string retention_destination_code;

    /**
     * @brief Whether the cut stands under legal hold.
     *
     * A held cut is never removed, whatever its retention says. The kind's legal_hold_default seeds
     * the value at the take, and a person sets it on one cut as its own act with its own reason.
     */
    bool legal_hold = false;

    /**
     * @brief Username of the person who last modified this cut.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const cut&, const cut&) = default;
};

/**
 * @brief Dispatch-key identifier for cut, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const cut&) {
    return "ores.reporting.cut";
}

}

#endif
