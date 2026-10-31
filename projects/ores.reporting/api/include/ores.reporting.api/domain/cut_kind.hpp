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
#ifndef ORES_REPORTING_API_DOMAIN_CUT_KIND_HPP
#define ORES_REPORTING_API_DOMAIN_CUT_KIND_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief The occasion a cut is taken for, and the four decisions the occasion carries.
 *
 * +#+updated: 2026-10-31
 *
 * Reference data table of the kinds of cut. A kind is not a sentence in a
 * knowledge page: it is a row a person maintains, because four of the facts it
 * carries are decisions a firm changes and reads back.
 *
 *
 * approval_kind_code is a nullable foreign key to the inbox's own lookup. It is
 * the whole of the authorisation rule: the end of day family's kinds point at
 * reporting.eod_cut_authorisation, whose decider is Finance, the verification
 * kind points at reporting.ipv_cut_authorisation, whose decider is Market Risk,
 * and the start of day, intraday and consumer kinds point at nothing. A screen
 * draws the words from the key's target and a dash where the key is null, so a
 * firm that gives the close to another function changes one approval kind and no
 * cut kind.
 *
 * Nothing a screen prints about authorisation is stored: the signed-or-not a
 * person reads is derived from this column.
 *
 * retention_months states the periods the design fixes: 25 for the ordinary
 * close and the open, 84 for the month end and the verification, 120 for the year
 * end, 3 for the intraday view and 1 for the live view. retention_destination_code
 * names where a cut goes when it leaves the warm tier. No storage tier lookup
 * exists yet, so the column carries a check on the codes this round knows and the
 * archive is a later task.
 */
struct cut_kind final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique kind code, the label's first segment.
     *
     * Examples: 'EOD', 'IPV', 'LIVE'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the kind.
     */
    std::string name;

    /**
     * @brief What the occasion is for, in the words a person new to the team reads.
     */
    std::string description;

    /**
     * @brief The family the kind belongs to, which fixes the body rule and the instant's source.
     */
    std::string family_code;

    /**
     * @brief The inbox kind that authorises a cut of this kind, or null where none does.
     *
     * The column is the whole of the authorisation rule. A cut minted under a kind whose key is
     * null raises no approval request, and the take enforces that rather than the store, because
     * the take is where the request would be written.
     */
    std::optional<std::string> approval_kind_code;

    /**
     * @brief Whether a result priced from a cut of the kind carries the indicative mark.
     *
     * The intraday and the consumer kinds are indicative; the close, the open and the verification
     * are official.
     */
    bool output_is_indicative = false;

    /**
     * @brief The rule by which a report resolves a selection of this kind to one cut.
     */
    std::string resolution_rule_code;

    /**
     * @brief How long a cut of the kind is kept before it leaves the warm tier.
     *
     * A cut copies this value when it is taken, so a later change to the kind does not move the
     * retention of a body already fixed.
     */
    int retention_months = 0;

    /**
     * @brief Where a cut of the kind goes when it leaves the warm tier.
     */
    std::string retention_destination_code;

    /**
     * @brief Whether a cut of the kind starts under legal hold.
     *
     * A held cut is never removed, whatever its retention says.
     */
    bool legal_hold_default = false;

    /**
     * @brief The order a person reads the kinds in, on the take and on the selection.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this cut kind.
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
    friend bool operator==(const cut_kind&, const cut_kind&) = default;
};

/**
 * @brief Dispatch-key identifier for cut_kind, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const cut_kind&) {
    return "ores.reporting.cut_kind";
}

}

#endif
