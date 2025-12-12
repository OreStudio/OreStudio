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
#ifndef ORES_COMMS_SERVICE_SESSION_SERVICE_HPP
#define ORES_COMMS_SERVICE_SESSION_SERVICE_HPP

#include <map>
#include <mutex>
#include <optional>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/messaging/message_types.hpp"

namespace ores::comms::service {

/**
 * @brief Information about an authenticated session.
 */
struct session_info {
    boost::uuids::uuid account_id;
    bool is_admin;
};

/**
 * @brief Centralized session management service.
 *
 * Tracks authenticated sessions by remote address. This service is shared
 * across all message handlers to provide consistent authorization checks.
 *
 * Thread-safety: All public methods are thread-safe.
 */
class session_service final {
private:
    inline static std::string_view logger_name =
        "ores.comms.service.session_service";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    session_service() = default;

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
     * @brief Check if a remote address has an admin session.
     *
     * @param remote_address The client's remote address
     * @return true if authenticated as admin, false otherwise
     */
    [[nodiscard]] bool is_admin(const std::string& remote_address) const;

    /**
     * @brief Store session for a remote address.
     *
     * @param remote_address The client's remote address
     * @param info Session information to store
     */
    void store_session(const std::string& remote_address, session_info info);

    /**
     * @brief Remove session for a remote address.
     *
     * @param remote_address The client's remote address
     */
    void remove_session(const std::string& remote_address);

    /**
     * @brief Remove all sessions (e.g., on server shutdown).
     */
    void clear_all_sessions();

    /**
     * @brief Check if a request is authorized based on message type and session.
     *
     * Centralizes authorization logic for all message types:
     * - Some messages don't require authentication (login, bootstrap, heartbeat)
     * - Some messages require authentication
     * - Some messages require admin privileges
     *
     * @param type The message type being requested
     * @param remote_address The client's remote address
     * @return Empty expected on success, error_code on failure:
     *         - authentication_failed if auth required but not logged in
     *         - authorization_failed if admin required but not admin
     */
    [[nodiscard]] std::expected<void, messaging::error_code>
    authorize_request(messaging::message_type type,
                      const std::string& remote_address) const;

private:
    /**
     * @brief Check if a message type requires authentication.
     */
    static bool requires_authentication(messaging::message_type type);

    /**
     * @brief Check if a message type requires admin privileges.
     */
    static bool requires_admin(messaging::message_type type);

    mutable std::mutex session_mutex_;
    std::map<std::string, session_info> sessions_;
};

}

#endif
