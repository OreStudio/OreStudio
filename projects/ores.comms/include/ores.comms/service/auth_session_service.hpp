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
#include <string>
#include <vector>
#include <optional>
#include <expected>
#include <functional>
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
     * @brief Initialize the sample delta baseline after login.
     *
     * Called once after the login response is written so that subsequent
     * sample deltas only reflect post-login traffic. Without this, the
     * first delta would include bytes accumulated by earlier sessions on
     * the same long-lived TCP connection.
     *
     * @param remote_address The client's remote address
     * @param bytes_sent Cumulative bytes sent at this moment
     * @param bytes_received Cumulative bytes received at this moment
     */
    void init_sample_baseline(const std::string& remote_address,
        std::uint64_t bytes_sent, std::uint64_t bytes_received);

    /**
     * @brief Record a time-series sample for an active session.
     *
     * Computes the delta bytes since the previous sample and appends it to
     * the session's accumulator. Every @c sample_flush_interval samples the
     * accumulator is moved to flush_pending for the handler to persist.
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
     * @brief A batch of samples ready to be flushed to the database.
     */
    struct pending_samples_batch {
        boost::uuids::uuid session_id;
        utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
        std::vector<session_sample> samples;
    };

    /**
     * @brief Take any samples ready to be flushed for a session.
     *
     * Returns and clears flush_pending for the given session. Returns nullopt
     * if the session is not found or has nothing pending.
     *
     * @param remote_address The client's remote address
     * @return batch with session_id, tenant_id and samples if pending, nullopt otherwise
     */
    [[nodiscard]] std::optional<pending_samples_batch>
    take_pending_samples(const std::string& remote_address);

    /// Number of samples accumulated before they are moved to flush_pending.
    /// At the default 30s heartbeat interval this gives a ~60s flush cadence.
    static constexpr std::size_t sample_flush_interval = 2;

    /**
     * @brief Update party context for an active session.
     *
     * Called after the client confirms party selection via select_party_request.
     *
     * @param remote_address The client's remote address
     * @param party_id The selected party UUID
     * @param visible_party_ids Party IDs visible to the selected party
     */
    void update_session_party(
        const std::string& remote_address,
        const boost::uuids::uuid& party_id,
        const std::vector<boost::uuids::uuid>& visible_party_ids);

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
     * @brief Result returned by the JWT validator function.
     *
     * Carries the two UUID strings extracted from a successfully decoded
     * token. Using a plain struct (no ores.security types) keeps the header
     * free of ores.security to avoid the circular dependency:
     * ores.comms ← ores.variability ← ores.comms.
     */
    struct jwt_validation_result {
        std::string account_id;  ///< UUID as string (JWT subject claim)
        std::string session_id;  ///< UUID as string (JWT session_id claim), empty if absent
    };

    /**
     * @brief Type alias for the JWT validation function.
     *
     * Takes a JWT string and returns a jwt_validation_result on success,
     * or nullopt on failure. Used to break the compile-time dependency
     * on ores.security from this header (ores.comms ← ores.variability ←
     * ores.comms would form a cycle if ores.security were linked directly).
     */
    using jwt_validator_fn = std::function<std::optional<jwt_validation_result>(const std::string&)>;

    /**
     * @brief Set the JWT validator used to validate bearer tokens in frames.
     *
     * The function receives a raw JWT string and returns a jwt_validation_result
     * on success, or nullopt on failure.
     * Optional: if not set, JWT validation is skipped (legacy mode).
     */
    void set_jwt_validator(jwt_validator_fn validator);

    /**
     * @brief Look up a session by its UUID session_id.
     *
     * Sessions are indexed by session_id when they are stored via
     * store_session_data() and when validate_jwt() succeeds. Returns nullopt
     * if the session_id is not found.
     *
     * @param session_id The session UUID (from the JWT session_id claim)
     * @return Session info if found, nullopt otherwise
     */
    [[nodiscard]] std::optional<session_info>
    get_session_by_session_id(const boost::uuids::uuid& session_id) const;

    /**
     * @brief Validate a JWT bearer token and refresh the session for remote_address.
     *
     * If the token is valid and a session for the extracted session_id is found,
     * the remote_address session entry is updated with the JWT-derived identity.
     * This allows JWT-carrying frames to re-hydrate their session from the token
     * rather than relying solely on the TCP remote address.
     *
     * @param jwt_string Raw JWT string from the frame
     * @param remote_address The client's remote address (used as session key)
     * @return session_info on success, nullopt if validation fails or no verifier set
     */
    [[nodiscard]] std::optional<session_info>
    validate_jwt(const std::string& jwt_string,
                 const std::string& remote_address);

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
    std::map<boost::uuids::uuid, std::string> session_id_index_; ///< session_id → remote_address

    mutable std::mutex client_info_mutex_;
    std::map<std::string, client_info> client_infos_;

    jwt_validator_fn jwt_validator_;
};

}

#endif
