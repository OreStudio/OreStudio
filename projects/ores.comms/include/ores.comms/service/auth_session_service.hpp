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
#include <vector>
#include <optional>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.comms/messaging/message_type.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/service/session_data.hpp"

namespace ores::comms::service {

/**
 * @brief Lightweight session info for authorization checks.
 *
 * Contains the essential identity information needed for request processing.
 * Authorization is handled via RBAC permissions checked at the handler level.
 */
struct session_info {
    boost::uuids::uuid account_id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    boost::uuids::uuid party_id = {};
    std::vector<boost::uuids::uuid> visible_party_ids;
    std::string username;
};

/**
 * @brief Client information captured during handshake.
 *
 * Stored by remote address after successful handshake, retrieved during
 * login to populate session tracking fields.
 */
struct client_info {
    std::string client_identifier;
    std::uint16_t client_version_major = 0;
    std::uint16_t client_version_minor = 0;
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
        using namespace ores::logging;
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
        std::shared_ptr<session_data> session);

    /**
     * @brief Get full session data for a remote address.
     *
     * @param remote_address The client's remote address
     * @return Shared pointer to session data, nullptr if not found
     */
    [[nodiscard]] std::shared_ptr<session_data>
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
     * @brief Record a time-series sample for an active session.
     *
     * Called at heartbeat frequency to capture cumulative byte counts and RTT.
     * Samples are stored in-memory and persisted to the database at logout.
     *
     * @param remote_address The client's remote address
     * @param bytes_sent Cumulative bytes sent at this moment
     * @param bytes_received Cumulative bytes received at this moment
     * @param latency_ms RTT reported by the client in this ping (ms), 0 if unknown
     */
    void record_sample(const std::string& remote_address,
        std::uint64_t bytes_sent, std::uint64_t bytes_received,
        std::uint64_t latency_ms = 0);

    /**
     * @brief Remove session for a remote address.
     *
     * @param remote_address The client's remote address
     * @return The removed session if found, nullptr otherwise
     */
    std::shared_ptr<session_data>
    remove_session(const std::string& remote_address);

    /**
     * @brief Remove all sessions (e.g., on server shutdown).
     *
     * @return All removed sessions
     */
    std::vector<std::shared_ptr<session_data>> clear_all_sessions();

    /**
     * @brief Get all active sessions.
     */
    [[nodiscard]] std::vector<std::shared_ptr<session_data>>
    get_all_sessions() const;

    /**
     * @brief Store client info from handshake.
     *
     * Called after successful handshake to store client details for later use
     * when creating a session during login.
     *
     * @param remote_address The client's remote address
     * @param info Client information from handshake
     */
    void store_client_info(const std::string& remote_address, client_info info);

    /**
     * @brief Get client info for a remote address.
     *
     * @param remote_address The client's remote address
     * @return Client info if stored, nullopt otherwise
     */
    [[nodiscard]] std::optional<client_info>
    get_client_info(const std::string& remote_address) const;

    /**
     * @brief Remove client info for a remote address.
     *
     * Called when connection is closed to clean up stored handshake data.
     *
     * @param remote_address The client's remote address
     */
    void remove_client_info(const std::string& remote_address);

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
    [[nodiscard]] std::expected<void, ores::utility::serialization::error_code>
    authorize_request(messaging::message_type type,
                      const std::string& remote_address) const;

private:
    /**
     * @brief Check if a message type requires authentication.
     */
    static bool requires_authentication(messaging::message_type type);

    mutable std::mutex session_mutex_;
    std::map<std::string, std::shared_ptr<session_data>> sessions_;

    mutable std::mutex client_info_mutex_;
    std::map<std::string, client_info> client_infos_;
};

}

#endif
