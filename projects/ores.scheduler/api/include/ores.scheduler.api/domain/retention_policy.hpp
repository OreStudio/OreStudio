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
#ifndef ORES_SCHEDULER_API_DOMAIN_RETENTION_POLICY_HPP
#define ORES_SCHEDULER_API_DOMAIN_RETENTION_POLICY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::scheduler::domain {

/**
 * @brief One store the platform keeps, and the retention period and action that bound it.
 *
 * A retention policy declares one bounded store: what it targets, what action is
 * taken, and the period past which the action applies. The target is named by
 * shape rather than by a particular table, so the policy survives a table being
 * replaced.
 *
 * A policy is a row of the installation, held by the system tenant: the
 * classification that governs published rows makes the platform the author of the
 * stores it bounds, and one copy in the system tenant is what keeps two
 * installations' policies from disagreeing. The column is not null for that
 * reason: a policy with no tenant has no owner to be published to.
 *
 * A policy is data, not a schema constant, so a period changes without a rebuild.
 * The scheduler fires one sweep, which applies the enabled policies that are due,
 * including where the storage engine runs no retention job of its own. A policy is
 * due once its cadence has passed since the sweep last applied it, so the sweep's
 * own schedule sets the finest cadence the installation can express.
 */
struct retention_policy final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique code for the policy.
     *
     * Examples: 'report_cashflows_365d', 'compute_node_caches_30d'.
     */
    std::string code;

    /**
     * @brief The shape of what the policy bounds: a chunked table, an ordinary table or an object
     * store key prefix.
     */
    std::string target_kind;

    /**
     * @brief The table name or key prefix the policy bounds.
     */
    std::string target;

    /**
     * @brief What the sweep does to the target past the period: drop its chunks, delete its rows,
     * or expire its objects.
     */
    std::string action;

    /**
     * @brief The period, in days, after which the action applies to the target.
     */
    int retention_days = 0;

    /**
     * @brief The hours between two applications of this policy. The sweep fires on the scheduler's
     * job schedule and applies a policy only once this period has passed since it last applied it,
     * so a store whose contents stop being useful over a month is not swept every day.
     */
    int cadence_hours = 0;

    /**
     * @brief Whether the sweep applies this policy.
     */
    bool enabled = true;

    /**
     * @brief Why the store keeps what it keeps for that period.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this retention policy.
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
    friend bool operator==(const retention_policy&, const retention_policy&) = default;
};

/**
 * @brief Dispatch-key identifier for retention_policy, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const retention_policy&) {
    return "ores.scheduler.retention_policy";
}

}

#endif
