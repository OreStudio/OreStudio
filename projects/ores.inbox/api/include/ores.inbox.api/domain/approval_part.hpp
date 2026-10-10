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
#ifndef ORES_INBOX_API_DOMAIN_APPROVAL_PART_HPP
#define ORES_INBOX_API_DOMAIN_APPROVAL_PART_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::inbox::domain {

/**
 * @brief One function whose approval a request may need, and the permission that answers for it.
 *
 * Reference data table of the parts of an approval: the functions whose approval
 * a request may need. Examples: 'controller', 'finance', 'market_risk',
 * 'operations'.
 *
 * A part is what makes a request need several different people. Each part names the
 * permission a person needs to answer for it, and its answer_order: a part with a
 * lower order must approve before a part with a higher one can be answered, and parts
 * with the same order are answered in parallel. A request's parts are the distinct
 * parts of its changes, and the request is approved only when every one of them has
 * an approval. A refusal from a part that can be answered ends the request.
 *
 * Parts are managed by the system tenant. A tenant without a function grants the
 * part's permission to whichever role does that work, so the part is decided by a
 * permission and not by a job title.
 */
struct approval_part final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique part code.
     *
     * Examples: 'controller', 'market_risk'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the part.
     */
    std::string name;

    /**
     * @brief What the part is responsible for.
     */
    std::string description;

    /**
     * @brief The permission a person needs to answer for this part.
     *
     * Examples: 'inbox::approvals:decide_finance'.
     */
    std::string decide_permission_code;

    /**
     * @brief The place of the part in the order of answers. A part can be answered only once every
     * part with a lower order has approved. Parts with the same order are answered in parallel.
     */
    int answer_order = 0;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this approval part.
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
    friend bool operator==(const approval_part&, const approval_part&) = default;
};

/**
 * @brief Dispatch-key identifier for approval_part, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const approval_part&) {
    return "ores.inbox.approval_part";
}

}

#endif
