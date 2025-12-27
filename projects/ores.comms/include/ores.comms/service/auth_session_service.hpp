/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_COMMS_SERVICE_AUTH_SESSION_SERVICE_HPP
#define ORES_COMMS_SERVICE_AUTH_SESSION_SERVICE_HPP

#include <map>
#include <mutex>
#include <memory>
#include <optional>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.iam/domain/session.hpp"

namespace ores::comms::service {

/**
 * @brief Lightweight session info for backward compatibility.
 *
 * Authorization is now handled via RBAC permissions checked at the handler
 * level using authorization_service.
 *
 * @deprecated Use get_session_data() to access the full session object.
 */
struct session_info {
    boost::uuids::uuid account_id;
};

/**
 * @brief Centralized authentication session management service.
 *
 * Tracks authenticated sessions by remote address. This service is shared
 * across all message handlers to provide consistent authorization checks.
 *
 * Thread-safety: All public methods are thread-safe.
 */
class auth_session_service final {
private:
    inline static std::string_view logger_name =
        "ores.comms.service.auth_session_service";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    auth_session_service() = default;

    /**
     * @brief Get session for a remote address.
     *
     * @param remote_address The client's remote address
     * @return Session info if logged in, nullopt otherwise
     */
    [[nodiscard]] std::optional<session_info>
    get_session(const std::string& remote_address) const;

    /**
     * @brief Check if a remote address has an authenticated session.
     *
     * @param remote_address The client's remote address
     * @return true if authenticated, false otherwise
     */
    [[nodiscard]] bool is_authenticated(const std::string& remote_address) const;

    /**
     * @brief Store session for a remote address (legacy interface).
     *
     * @param remote_address The client's remote address
     * @param info Session information to store
     * @deprecated Use store_session_data() for full session tracking.
     */
    void store_session(const std::string& remote_address, session_info info);

    /**
     * @brief Store full session data for a remote address.
     *
     * @param remote_address The client's remote address
     * @param session Full session object with all tracking data
     */
    void store_session_data(const std::string& remote_address,
        std::shared_ptr<iam::domain::session> session);

    /**
     * @brief Get full session data for a remote address.
     *
     * @param remote_address The client's remote address
     * @return Shared pointer to session data, nullptr if not found
     */
    [[nodiscard]] std::shared_ptr<iam::domain::session>
    get_session_data(const std::string& remote_address) const;

    /**
     * @brief Update byte counters for an active session.
     *
     * @param remote_address The client's remote address
     * @param bytes_sent Total bytes sent on the connection
     * @param bytes_received Total bytes received on the connection
     */
    void update_session_bytes(const std::string& remote_address,
        std::uint64_t bytes_sent, std::uint64_t bytes_received);

    /**
     * @brief Remove session for a remote address.
     *
     * @param remote_address The client's remote address
     * @return The removed session if found, nullptr otherwise
     */
    std::shared_ptr<iam::domain::session>
    remove_session(const std::string& remote_address);

    /**
     * @brief Remove all sessions (e.g., on server shutdown).
     *
     * @return All removed sessions
     */
    std::vector<std::shared_ptr<iam::domain::session>> clear_all_sessions();

    /**
     * @brief Get all active sessions.
     */
    [[nodiscard]] std::vector<std::shared_ptr<iam::domain::session>>
    get_all_sessions() const;

    /**
     * @brief Check if a request is authorized based on message type and session.
     *
     * Centralizes authentication logic for all message types:
     * - Some messages don't require authentication (login, bootstrap, heartbeat)
     * - All other messages require authentication
     *
     * Note: Permission-based authorization is handled at the handler level
     * using authorization_service.has_permission().
     *
     * @param type The message type being requested
     * @param remote_address The client's remote address
     * @return Empty expected on success, error_code on failure:
     *         - authentication_failed if auth required but not logged in
     */
    [[nodiscard]] std::expected<void, messaging::error_code>
    authorize_request(messaging::message_type type,
                      const std::string& remote_address) const;

private:
    /**
     * @brief Check if a message type requires authentication.
     */
    static bool requires_authentication(messaging::message_type type);

    mutable std::mutex session_mutex_;
    std::map<std::string, std::shared_ptr<iam::domain::session>> sessions_;
};

}

#endif
