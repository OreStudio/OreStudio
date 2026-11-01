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
#ifndef ORES_REPORTING_API_DOMAIN_CUT_APPROVAL_DETAIL_HPP
#define ORES_REPORTING_API_DOMAIN_CUT_APPROVAL_DETAIL_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief The cut an approval request authorises, with the counts of the body's parts.
 *
 * The detail row of one reporting.eod_cut_authorisation or
 * reporting.ipv_cut_authorisation approval request: the cut the request
 * authorises, and the counts of the body the decider is asked to sign.
 *
 * A request is a header. What it is about lives here, keyed by the request's own
 * id, exactly as a proposed published set lives in
 * ores.marketdata.curve_publish and is keyed by its request. The subject is one
 * fact and no more: the cut whose body was frozen. The counts travel beside it
 * because the decider reads them before answering and the cut's own record states
 * them only by reading the body's parts one at a time: the deals the population
 * held at the instant, the published sets in use at it, and the gaps that would
 * have stopped the take, which is zero for any body a request was raised for.
 *
 * applied_at and applied_by are the record of the apply. Both are empty while
 * the request waits, and both are written in one act when the approved request is
 * applied to the cut: the instant the authorisation was stamped and the account
 * that stamped it. The row is bitemporal, so the applied version is a new version
 * of the same row and the history holds the unapplied subject beside it.
 * request_id is the natural key, so a request authorises one cut and no second
 * apply can find a row to write.
 *
 * The row is not a client's to write: the take writes it with the body, and the
 * apply stamps it. A client reads it to decide a request or to audit a cut.
 */
struct cut_approval_detail final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this detail row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The approval request this row is the subject of.
     *
     * One request, one cut: the row is written when the request is raised and stamped when it is
     * applied, so a request that authorised one body has no second row to authorise another.
     */
    boost::uuids::uuid request_id;

    /**
     * @brief The party whose body the cut fixed.
     *
     * The subject names it because the same cut label in two parties is two bodies, and a decision
     * on one must not authorise the other.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The cut whose body the request authorises.
     *
     * The cut is named and not copied: the body is the cut's own record, so the decider's page
     * reads the body through this key and the two records cannot disagree about the instant the
     * body was frozen at.
     */
    boost::uuids::uuid cut_id;

    /**
     * @brief How many deals the population held at the cut's instant.
     *
     * The count is the assembly's, taken when the body was frozen, so a decider reads the size of
     * the population the take fixed rather than the store's size today.
     */
    int deal_count = 0;

    /**
     * @brief How many published sets were in use at the cut's instant.
     *
     * The count is the assembly's, taken when the body was frozen. A body with none is a body whose
     * valuation has no curve to price from, so a request names none only when the take recorded it
     * as a gap and raised nothing.
     */
    int market_data_set_count = 0;

    /**
     * @brief How many parts of the body the assembly could not fill.
     *
     * Zero for every body a request was raised for, because the take raises no request while a gap
     * stands. The column is held anyway: a decider reads that the question of completeness was
     * asked and answered, and not that nobody asked it.
     */
    int gap_count = 0;

    /**
     * @brief The account that applied the approved request to the cut, or empty while it waits.
     *
     * The account is the applier, which is not necessarily the decider: the approval records who
     * answered, and this records who stamped the cut.
     */
    std::string applied_by = "";

    /**
     * @brief When the approved request was applied to the cut, or null while it waits.
     *
     * This is the applied marker: a request is applied when a detail row for it holds an instant
     * here, and the apply is idempotent because a row that already holds one is a row whose work is
     * done.
     */
    std::optional<std::chrono::system_clock::time_point> applied_at;

    /**
     * @brief Username of the person who last modified this cut approval detail.
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
    friend bool operator==(const cut_approval_detail&, const cut_approval_detail&) = default;
};

/**
 * @brief Dispatch-key identifier for cut_approval_detail, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const cut_approval_detail&) {
    return "ores.reporting.cut_approval_detail";
}

}

#endif
