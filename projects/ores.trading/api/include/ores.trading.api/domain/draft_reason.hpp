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
#ifndef ORES_TRADING_API_DOMAIN_DRAFT_REASON_HPP
#define ORES_TRADING_API_DOMAIN_DRAFT_REASON_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Why a trade is a draft, and which desk must unblock it.
 *
 * A trade that is not live is a draft, and a draft with no stated reason is a deal
 * nobody can pick up: the desk sees a row that has not moved and cannot tell whether
 * it waits on the customer, on a confirmation, or on the rest of a structure.
 *
 * The lookup states the reason, and the desk that owns it. The reason is a code the
 * booking names, so a draft's reason is reference data rather than free text, and
 * the responsible desk is what a report groups drafts by when it asks who is late.
 */
struct draft_reason final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The reason a draft exists, as the booking names it.
     *
     * Examples: 'structure_incomplete', 'confirmation_pending', 'credit_review'.
     */
    std::string code;

    /**
     * @brief What the draft waits on, in the words a person reads.
     */
    std::string description;

    /**
     * @brief The desk that unblocks a draft held for this reason, as the role catalogue names it. A
     * report of drafts groups by this, which is what makes an aged draft somebody's work rather
     * than nobody's.
     */
    std::string responsible_role;

    /**
     * @brief Username of the person who last modified this draft reason.
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
    friend bool operator==(const draft_reason&, const draft_reason&) = default;
};

/**
 * @brief Dispatch-key identifier for draft_reason, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const draft_reason&) {
    return "ores.trading.draft_reason";
}

}

#endif
