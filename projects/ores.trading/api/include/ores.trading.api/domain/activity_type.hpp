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
#ifndef ORES_TRADING_API_DOMAIN_ACTIVITY_TYPE_HPP
#define ORES_TRADING_API_DOMAIN_ACTIVITY_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Internal trade activity classification (e.g. new_booking, amendment, novation).
 *
 * Internal trade activity classification. Each activity type states what
 * happened to a trade (e.g. new_booking, amendment, novation). It optionally
 * maps to an FpML event type code for wire-format messages, and optionally
 * links to an FSM transition that drives the trade's operational status
 * change.
 *
 * The table is bi-temporal and audited: it carries version, the four audit
 * columns and the valid_from/valid_to pair with the GIST exclusion and the
 * delete rule, so the model takes the ordinary audited shape and needs no
 * shape flag.
 */
struct activity_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique activity type code.
     *
     * Examples: 'new_booking', 'amendment', 'novation'.
     */
    std::string code;

    /**
     * @brief High-level category grouping this activity type.
     *
     * One of: new_activity, lifecycle_event, misbooking, valuation_change, cancellation.
     *
     * Soft FK to ores_trading_activity_categories_tbl: the values are the closed activity category
     * set. PR 4 tightens the soft reference into a real foreign key.
     */
    std::string category;

    /**
     * @brief True when this activity type requires counterparty confirmation. This is the
     * /confirmable/ axis of [[id:584EC77C-9BA2-4200-9112-FBA1B6A2190D][Trade activity]]: a
     * confirmable activity changes the terms the counterparty agreed, so a new confirmation is
     * issued.
     */
    bool requires_confirmation = false;

    /**
     * @brief True when an activity of this type changes what the trade pays, so the trade must be
     * revalued. This is the /economic/ axis.
     */
    bool is_economic = false;

    /**
     * @brief True when an activity of this type changes anything at all. This is the /real/ axis.
     * Only a null amend is not real: the value entered equals the value held, so the activity is
     * recorded for the audit trail and versions nothing.
     */
    bool is_real = true;

    /**
     * @brief Where this activity type sits when several causes land on one version: the lowest
     * number names the version. Ranks 1 to 8 follow the order in
     * [[id:8BC3A226-6DD8-49C8-959A-F1AAC97A3F97][Trade versioning]] (book moves, funding roll or
     * reserve, CEM charge or contra revenue, close outs, triggered, exercised or expired, fixing,
     * misbooking); 9 is every other cause.
     */
    int priority = 0;

    /**
     * @brief Detailed description of the activity type.
     */
    std::string description;

    /**
     * @brief Optional FpML event type code for wire-format mapping.
     *
     * Soft FK to ores_trading_fpml_event_types_tbl. Empty when no FpML equivalent is stated.
     */
    std::string fpml_event_type_code;

    /**
     * @brief Optional FSM transition that this activity triggers.
     *
     * Soft FK to ores_dq_fsm_transitions_tbl. Null when the activity drives no status change.
     */
    std::optional<boost::uuids::uuid> fsm_transition_id;

    /**
     * @brief Username of the person who last modified this activity type.
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
    friend bool operator==(const activity_type&, const activity_type&) = default;
};

/**
 * @brief Dispatch-key identifier for activity_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const activity_type&) {
    return "ores.trading.activity_type";
}

}

#endif
