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
#ifndef ORES_INBOX_API_DOMAIN_APPROVAL_KIND_HPP
#define ORES_INBOX_API_DOMAIN_APPROVAL_KIND_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::inbox::domain {

/**
 * @brief What an approval request asks for, and the rules that decide it.
 *
 * Reference data table of the kinds of approval request. Examples:
 * 'iam.role_grant', 'trading.operational_authorisation'.
 *
 * A kind is the small declaration the generic record reads: the permission a
 * decider needs, whether the kind uses the held state, whether an approval
 * needs a comment, how long an unanswered request waits, and how many approvals,
 * each by a different person, close a request as approved. The component that
 * owns the kind owns its detail table and the operation that applies an approval.
 *
 * Kinds are managed by the system tenant.
 */
struct approval_kind final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique kind code, prefixed by the component that owns the kind.
     *
     * Examples: 'iam.role_grant'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the kind.
     */
    std::string name;

    /**
     * @brief What the kind is for.
     */
    std::string description;

    /**
     * @brief The permission a person needs to decide a request of this kind.
     *
     * Examples: 'iam::roles:assign'.
     */
    std::string decide_permission_code;

    /**
     * @brief Whether held is a state this kind uses.
     */
    bool allows_hold = false;

    /**
     * @brief Whether an approval needs a comment. A refusal always needs one.
     */
    bool comment_on_approve = false;

    /**
     * @brief The number of days after which an unanswered request closes as expired, or null when a
     * request of this kind does not expire.
     */
    std::optional<int> expires_after_days;

    /**
     * @brief How many approvals, each by a different person, close a request as approved.
     */
    int approvals_required = 0;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this approval kind.
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
    friend bool operator==(const approval_kind&, const approval_kind&) = default;
};

/**
 * @brief Dispatch-key identifier for approval_kind, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const approval_kind&) {
    return "ores.inbox.approval_kind";
}

}

#endif
