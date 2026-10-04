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
#ifndef ORES_INBOX_API_DOMAIN_APPROVAL_REQUEST_HPP
#define ORES_INBOX_API_DOMAIN_APPROVAL_REQUEST_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::inbox::domain {

/**
 * @brief A request that waits for a person to decide it.
 *
 * The header every kind of approval request shares: what is asked, who asked,
 * when and why, where the request is, and when it closes unanswered. What the
 * request is about lives in the kind's own detail table, owned by the component
 * that owns the kind and keyed by this request's id.
 *
 * state_code is the state the request's decisions have reached. The decisions
 * themselves are rows of [[id:9B3D6E81-4F2A-47C5-8E19-0A5C7D3B2E96][ores.inbox.approval_decision]],
 * many per request.
 *
 * The record is bitemporal, so every change of state is a version and the history
 * is the audit trail an auditor reads.
 */
struct approval_request final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID primary key for the request.
     */
    boost::uuids::uuid id;

    /**
     * @brief What is asked. FK reference to ores_inbox_approval_kinds_tbl, which the system tenant
     * holds.
     */
    std::string kind_code;

    /**
     * @brief Where the request is. FK reference to ores_inbox_approval_request_states_tbl, which
     * the system tenant holds.
     */
    std::string state_code;

    /**
     * @brief The account that asked. A system may ask on a person's behalf, through its own service
     * account. FK reference to ores_iam_accounts_tbl.
     */
    boost::uuids::uuid requested_by;

    /**
     * @brief When the request was made.
     */
    std::chrono::system_clock::time_point requested_at = {};

    /**
     * @brief Why the person asks, in their words.
     */
    std::string reason;

    /**
     * @brief When the request closes unanswered, or null when it does not expire.
     */
    std::optional<std::chrono::system_clock::time_point> expires_at;

    /**
     * @brief Username of the person who last modified this approval request.
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
    friend bool operator==(const approval_request&, const approval_request&) = default;
};

/**
 * @brief Dispatch-key identifier for approval_request, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const approval_request&) {
    return "ores.inbox.approval_request";
}

}

#endif
