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
#ifndef ORES_IAM_API_DOMAIN_AUTH_EVENT_HPP
#define ORES_IAM_API_DOMAIN_AUTH_EVENT_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief One authentication event: a login, a failed attempt, a logout, a token refresh, a rejected
 * refresh, a tenant entered or left, or a signup; a TimescaleDB hypertable partitioned by
 * event_time.
 *
 * One row per authentication event. The rows are append-only, so the model states
 * :current_state:: an event is a fact about an instant, and the bi-temporal pair
 * has no meaning on it.
 *
 * The table is the audit log of who signed in and who failed to. Its tenant_id
 * is an ordinary text column and not the generated kind, because an event can
 * happen before a tenant is known, and it is then the system tenant's. The read in
 * ores.iam.auth_event_operations scopes on that column itself.
 *
 * The table is internal: its protocol, handler and shell commands are the
 * hand-written operation model's, so only the data layer and the SQL schema are
 * generated here.
 *
 * The table is isolated by tenant like the rest of IAM, by a policy in
 * iam_rls_policies_create.sql. An event that names no tenant is the system
 * tenant's. The hourly and daily continuous aggregates it once had are gone:
 * nothing read them, and TimescaleDB does not build a continuous aggregate over a
 * table with row-level security.
 */
struct auth_event final {
    /**
     * @brief The event's id, a uuid in its text form. It sits in the primary key beside the
     * partition column.
     */
    std::string id;

    /**
     * @brief When the event happened. The partition column.
     */
    std::chrono::system_clock::time_point event_time;

    /**
     * @brief The tenant the event belongs to, in its text form. Empty when the event came before a
     * tenant was resolved, such as a failed login.
     */
    std::string tenant_id;

    /**
     * @brief The account the event is about, in its text form. Empty when unknown.
     */
    std::string account_id;

    /**
     * @brief What happened: login_success, login_failure, logout, token_refresh,
     * max_session_exceeded, tenant_entered, tenant_left, signup_success or signup_failure.
     */
    std::string event_type;

    /**
     * @brief The username the event names. Empty for an event with no resolved account.
     */
    std::string username;

    /**
     * @brief The session the event belongs to. Empty when no session was established.
     */
    std::string session_id;

    /**
     * @brief The party chosen at sign-in. Empty when none was.
     */
    std::string party_id;

    /**
     * @brief Why a failure failed. Empty for a success.
     */
    std::string error_detail;

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
    friend bool operator==(const auth_event&, const auth_event&) = default;
};

/**
 * @brief Dispatch-key identifier for auth_event, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const auth_event&) {
    return "ores.iam.auth_event";
}

}

#endif
