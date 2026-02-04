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
#ifndef ORES_IAM_SERVICE_SERVICE_SESSION_SERVICE_HPP
#define ORES_IAM_SERVICE_SERVICE_SESSION_SERVICE_HPP

#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.iam/domain/session.hpp"
#include "ores.iam/repository/session_repository.hpp"
#include "ores.iam/repository/account_repository.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::iam::service {

/**
 * @brief Service for managing service account sessions.
 *
 * Service accounts (service, algorithm, llm) cannot login with passwords.
 * They create sessions directly at startup and end them on shutdown.
 * This service provides a simplified interface for managing these sessions.
 */
class service_session_service {
private:
    inline static std::string_view logger_name =
        "ores.iam.service.service_session_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a service_session_service with required repositories.
     *
     * @param ctx Database context for persistence operations.
     */
    explicit service_session_service(context ctx);

    /**
     * @brief Starts a session for a service account.
     *
     * Creates a new session record for the specified service account.
     * Service accounts are identified by username and must have a non-user
     * account type.
     *
     * @param username The username of the service account.
     * @param client_identifier The client identifier (e.g., "ores.service.binary").
     * @param protocol The session protocol (binary or http).
     * @return The created session if successful, nullopt if account not found
     *         or account is not a service account.
     */
    std::optional<domain::session> start_service_session(
        const std::string& username,
        const std::string& client_identifier,
        domain::session_protocol protocol = domain::session_protocol::binary);

    /**
     * @brief Starts a session for a service account by account ID.
     *
     * @param account_id The UUID of the service account.
     * @param client_identifier The client identifier.
     * @param protocol The session protocol.
     * @return The created session if successful, nullopt if account not found
     *         or account is not a service account.
     */
    std::optional<domain::session> start_service_session(
        const boost::uuids::uuid& account_id,
        const std::string& client_identifier,
        domain::session_protocol protocol = domain::session_protocol::binary);

    /**
     * @brief Ends a service session.
     *
     * Sets the end_time on the session record.
     *
     * @param session_id The UUID of the session to end.
     * @param start_time The session start time (required for TimescaleDB lookup).
     * @param bytes_sent Final bytes sent count (default 0).
     * @param bytes_received Final bytes received count (default 0).
     */
    void end_service_session(
        const boost::uuids::uuid& session_id,
        const std::chrono::system_clock::time_point& start_time,
        std::uint64_t bytes_sent = 0,
        std::uint64_t bytes_received = 0);

    /**
     * @brief Looks up a service account by username.
     *
     * @param username The username to look up.
     * @return The account if found and is a service account, nullopt otherwise.
     */
    std::optional<domain::account> get_service_account(const std::string& username);

private:
    repository::session_repository session_repo_;
    repository::account_repository account_repo_;
    utility::uuid::uuid_v7_generator uuid_generator_;
};

}

#endif
