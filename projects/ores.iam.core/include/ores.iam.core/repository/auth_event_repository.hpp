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
#ifndef ORES_IAM_REPOSITORY_AUTH_EVENT_REPOSITORY_HPP
#define ORES_IAM_REPOSITORY_AUTH_EVENT_REPOSITORY_HPP

#include <chrono>
#include <string>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::iam::repository {

/**
 * @brief Repository for inserting JWT authentication telemetry events.
 *
 * Writes to the ores_iam_auth_events_tbl TimescaleDB hypertable.
 * Insert-only — events are immutable once recorded.
 *
 * This is a system-level audit log: no RLS is applied. The caller must
 * ensure that the context has write access to the auth events table.
 */
class auth_event_repository {
private:
    inline static std::string_view logger_name =
        "ores.iam.repository.auth_event_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;
    explicit auth_event_repository(context ctx);

    /**
     * @brief Record a login success event.
     *
     * @param event_time  Timestamp of the event
     * @param tenant_id   Tenant UUID string
     * @param account_id  Account UUID string
     * @param username    Username used for login
     * @param session_id  Session UUID string created at login
     * @param party_id    Selected party UUID string (empty if multi-party)
     */
    void record_login_success(
        const std::chrono::system_clock::time_point& event_time,
        const std::string& tenant_id,
        const std::string& account_id,
        const std::string& username,
        const std::string& session_id,
        const std::string& party_id);

    /**
     * @brief Record a login failure event.
     *
     * @param event_time   Timestamp of the event
     * @param tenant_id    Tenant UUID string (empty if tenant could not be resolved)
     * @param username     Username that was attempted
     * @param error_detail Error message describing the failure reason
     */
    void record_login_failure(
        const std::chrono::system_clock::time_point& event_time,
        const std::string& tenant_id,
        const std::string& username,
        const std::string& error_detail);

    /**
     * @brief Record a logout event.
     *
     * @param event_time  Timestamp of the event
     * @param tenant_id   Tenant UUID string
     * @param account_id  Account UUID string
     * @param username    Username of the account
     * @param session_id  Session UUID string being ended
     */
    void record_logout(
        const std::chrono::system_clock::time_point& event_time,
        const std::string& tenant_id,
        const std::string& account_id,
        const std::string& username,
        const std::string& session_id);

    /**
     * @brief Record a successful token refresh event.
     *
     * @param event_time  Timestamp of the event
     * @param tenant_id   Tenant UUID string from the JWT claims
     * @param account_id  Account UUID string (subject claim)
     * @param username    Username from the JWT claims
     * @param session_id  Session UUID string from the JWT claims
     */
    void record_token_refresh(
        const std::chrono::system_clock::time_point& event_time,
        const std::string& tenant_id,
        const std::string& account_id,
        const std::string& username,
        const std::string& session_id);

    /**
     * @brief Record a max_session_exceeded event.
     *
     * Emitted when a token refresh is rejected because the session has
     * reached its maximum allowed duration.
     *
     * @param event_time  Timestamp of the event
     * @param tenant_id   Tenant UUID string from the JWT claims
     * @param account_id  Account UUID string (subject claim)
     * @param username    Username from the JWT claims
     * @param session_id  Session UUID string from the JWT claims
     */
    void record_max_session_exceeded(
        const std::chrono::system_clock::time_point& event_time,
        const std::string& tenant_id,
        const std::string& account_id,
        const std::string& username,
        const std::string& session_id);

    /**
     * @brief Record a signup success event.
     *
     * @param event_time  Timestamp of the event
     * @param tenant_id   Tenant UUID string
     * @param account_id  New account UUID string
     * @param username    Username created
     */
    void record_signup_success(
        const std::chrono::system_clock::time_point& event_time,
        const std::string& tenant_id,
        const std::string& account_id,
        const std::string& username);

    /**
     * @brief Record a signup failure event.
     *
     * @param event_time   Timestamp of the event
     * @param tenant_id    Tenant UUID string
     * @param username     Username that was attempted
     * @param error_detail Error message describing the failure reason
     */
    void record_signup_failure(
        const std::chrono::system_clock::time_point& event_time,
        const std::string& tenant_id,
        const std::string& username,
        const std::string& error_detail);

private:
    void insert(const std::string& event_type,
        const std::chrono::system_clock::time_point& event_time,
        const std::string& tenant_id,
        const std::string& account_id,
        const std::string& username,
        const std::string& session_id,
        const std::string& party_id,
        const std::string& error_detail);

    context ctx_;
};

}

#endif
