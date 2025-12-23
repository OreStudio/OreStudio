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
#ifndef ORES_QT_CLIENT_MANAGER_HPP
#define ORES_QT_CLIENT_MANAGER_HPP

#include <chrono>
#include <memory>
#include <optional>
#include <string>
#include <thread>
#include <boost/asio/io_context.hpp>
#include <boost/asio/executor_work_guard.hpp>
#include <boost/uuid/uuid.hpp>
#include <QObject>
#include <QDateTime>
#include "ores.comms/net/client.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.comms/service/remote_event_adapter.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.iam/domain/session.hpp"

namespace ores::qt {

/**
 * @brief Result of a login attempt.
 */
struct LoginResult {
    bool success = false;
    QString error_message;
    bool password_reset_required = false;
};

/**
 * @brief Result of a signup attempt.
 */
struct SignupResult {
    bool success = false;
    QString error_message;
    QString username;
};

/**
 * @brief Result of a session list request.
 */
struct SessionListResult {
    std::vector<iam::domain::session> sessions;
    std::uint32_t total_count = 0;
};

/**
 * @brief Manages the lifecycle of the network client and IO context.
 *
 * Maintains a persistent IO context/thread while allowing the client connection
 * to be established and torn down repeatedly. Signals changes in connection
 * state to allow UI components to update accordingly without closing.
 */
class ClientManager : public QObject {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_manager";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ClientManager(std::shared_ptr<eventing::service::event_bus> event_bus,
                           QObject* parent = nullptr);
    ~ClientManager() override;

    /**
     * @brief Connect to the server and perform login.
     *
     * @param host Server hostname
     * @param port Server port
     * @param username Login username
     * @param password Login password
     * @return LoginResult containing success status, error message, and password_reset_required flag
     */
    LoginResult connectAndLogin(
        const std::string& host,
        std::uint16_t port,
        const std::string& username,
        const std::string& password);

    /**
     * @brief Connect to the server and attempt signup.
     *
     * Creates a temporary connection to register a new user account.
     * Does not establish a persistent connection or log in the user.
     * The connection is closed after the signup attempt completes.
     *
     * @param host Server hostname
     * @param port Server port
     * @param username Desired username
     * @param email User's email address
     * @param password Desired password
     * @return SignupResult containing success status and error message if failed
     */
    SignupResult signup(
        const std::string& host,
        std::uint16_t port,
        const std::string& username,
        const std::string& email,
        const std::string& password);

    /**
     * @brief Logout the current user and disconnect from the server.
     *
     * Sends a logout request to mark the user as offline before disconnecting.
     */
    void disconnect();

    /**
     * @brief Logout the current user without disconnecting.
     *
     * Sends a logout request to the server to mark the user as offline.
     * @return true if logout was successful, false otherwise
     */
    bool logout();

    /**
     * @brief Check if currently connected.
     */
    bool isConnected() const;

    /**
     * @brief Check if the logged-in user has admin privileges.
     *
     * @deprecated Permission checks are now performed server-side via RBAC.
     *             This method always returns false.
     * @return Always returns false. Use server-side permission checks instead.
     */
    [[deprecated("Permission checks are now server-side via RBAC")]]
    bool isAdmin() const;

    /**
     * @brief Check if currently logged in.
     *
     * @return true if logged in, false otherwise.
     */
    bool isLoggedIn() const { return session_.is_logged_in(); }

    /**
     * @brief Get the current logged-in user's username.
     *
     * @return Username string, or empty if not logged in.
     */
    std::string currentUsername() const { return session_.username(); }

    /**
     * @brief Get the current logged-in user's email.
     *
     * @return Email string, or empty if not logged in or not set.
     */
    std::string currentEmail() const { return session_.email(); }

    /**
     * @brief Set the current logged-in user's email.
     *
     * Used after successful email update to keep local state in sync.
     */
    void setCurrentEmail(const std::string& email) { session_.set_email(email); }

    /**
     * @brief Get the account ID if logged in.
     *
     * @return Account UUID if logged in, nullopt otherwise.
     */
    std::optional<boost::uuids::uuid> accountId() const { return session_.account_id(); }

    /**
     * @brief Send a request if connected.
     *
     * @param request The request frame to send
     * @return Response frame or error code
     * @deprecated Use typed process_request methods instead
     */
    std::expected<comms::messaging::frame, comms::messaging::error_code>
    sendRequest(comms::messaging::frame request);

    /**
     * @brief Process a request that does not require authentication.
     *
     * Uses message_traits to automatically determine the response type.
     *
     * @tparam RequestType Request message type (must have message_traits)
     * @param request The request to send
     * @return Response on success, error on failure
     */
    template <typename RequestType>
        requires comms::messaging::has_message_traits<RequestType>
    auto process_request(RequestType request) {
        return session_.process_request(std::move(request));
    }

    /**
     * @brief Process a request that requires authentication.
     *
     * Checks if logged in before sending.
     *
     * @tparam RequestType Request message type (must have message_traits)
     * @param request The request to send
     * @return Response on success, error on failure (including not_logged_in)
     */
    template <typename RequestType>
        requires comms::messaging::has_message_traits<RequestType>
    auto process_authenticated_request(RequestType request) {
        return session_.process_authenticated_request(std::move(request));
    }

    /**
     * @brief Process a request that requires admin privileges.
     *
     * Checks if logged in as admin before sending.
     *
     * @tparam RequestType Request message type (must have message_traits)
     * @param request The request to send
     * @return Response on success, error on failure
     */
    template <typename RequestType>
        requires comms::messaging::has_message_traits<RequestType>
    auto process_admin_request(RequestType request) {
        return session_.process_admin_request(std::move(request));
    }

    /**
     * @brief Get the underlying client session.
     *
     * Provides access to the session for advanced use cases.
     */
    comms::net::client_session& session() { return session_; }

    /**
     * @brief List sessions for an account.
     *
     * @param accountId The account UUID (nil for own sessions)
     * @param limit Maximum sessions to return
     * @param offset Pagination offset
     * @return Session list result or nullopt on error
     */
    std::optional<SessionListResult> listSessions(
        const boost::uuids::uuid& accountId,
        std::uint32_t limit = 100,
        std::uint32_t offset = 0);

    /**
     * @brief Get the current client (internal use only).
     */
    std::shared_ptr<comms::net::client> getClient() const { return client_; }

    /**
     * @brief Get the IO context executor.
     */
    boost::asio::any_io_executor getExecutor() {
        return io_context_->get_executor();
    }

    /**
     * @brief Subscribe to server-push notifications for an event type.
     *
     * This method is non-blocking - the subscription request is sent
     * asynchronously and any errors are logged.
     *
     * @param eventType The event type to subscribe to (e.g., "ores.risk.currency_changed_event")
     */
    void subscribeToEvent(const std::string& eventType);

    /**
     * @brief Unsubscribe from server-push notifications for an event type.
     *
     * This method is non-blocking - the unsubscription request is sent
     * asynchronously and any errors are logged.
     *
     * @param eventType The event type to unsubscribe from
     */
    void unsubscribeFromEvent(const std::string& eventType);

    /**
     * @brief Set the supported compression bitmask for client connections.
     *
     * Should be called before connectAndLogin(). The value is used when
     * creating client_options for the handshake negotiation.
     *
     * @param compression Bitmask of supported compression types (0 = disabled)
     */
    void setSupportedCompression(std::uint8_t compression) {
        supported_compression_ = compression;
    }

signals:
    void connected();
    void disconnected();
    void reconnecting();
    void reconnected();
    void connectionError(const QString& message);

    /**
     * @brief Emitted when a notification is received from the server.
     *
     * @param eventType The event type name (e.g., "ores.risk.currency_changed_event")
     * @param timestamp When the event occurred
     */
    void notificationReceived(const QString& eventType, const QDateTime& timestamp);

private:
    void setupIO();
    void cleanupIO();

private:
    // Persistent IO infrastructure
    std::unique_ptr<boost::asio::io_context> io_context_;
    std::unique_ptr<boost::asio::executor_work_guard<
        boost::asio::io_context::executor_type>> work_guard_;
    std::unique_ptr<std::thread> io_thread_;

    // Transient client (owned by ClientManager, attached to session_)
    std::shared_ptr<comms::net::client> client_;

    // Client session for auth-aware request handling and session state
    comms::net::client_session session_;

    // Event bus for publishing connection events (passed to client)
    std::shared_ptr<eventing::service::event_bus> event_bus_;

    // Connection details for event publishing
    std::string connected_host_;
    std::uint16_t connected_port_{0};

    // Compression support bitmask (default: all compression types)
    std::uint8_t supported_compression_{0x07}; // COMPRESSION_SUPPORT_ALL
};

}

#endif
