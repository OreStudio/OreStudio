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

#include <atomic>
#include <chrono>
#include <concepts>
#include <expected>
#include <memory>
#include <optional>
#include <string>
#include <filesystem>
#include <boost/uuid/uuid.hpp>
#include <rfl/json.hpp>
#include <QObject>
#include "ores.utility/rfl/reflectors.hpp"
#include <QDateTime>
#include "ores.shell/service/nats_session.hpp"
#include "ores.nats/service/jetstream_admin.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.iam/domain/session.hpp"
#include "ores.iam/messaging/session_samples_protocol.hpp"

namespace ores::qt {

/**
 * @brief Concept for NATS-aware request types.
 *
 * A type satisfies nats_request if it has a nats_subject and response_type.
 */
template <typename T>
concept nats_request = requires {
    { T::nats_subject } -> std::convertible_to<std::string_view>;
    typename T::response_type;
};

/**
 * @brief Summary of a party the user can select during login.
 */
struct PartyInfo {
    boost::uuids::uuid id;
    QString name;
};

/**
 * @brief Result of a login attempt.
 */
struct LoginResult {
    bool success = false;
    QString error_message;
    bool password_reset_required = false;
    bool bootstrap_mode = false;
    bool tenant_bootstrap_mode = false;
    boost::uuids::uuid selected_party_id;
    std::vector<PartyInfo> available_parties;
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
 * @brief Manages the lifecycle of the NATS client and login state.
 *
 * Maintains a persistent NATS connection while allowing the authentication
 * state to be established and torn down repeatedly. Signals changes in
 * connection state to allow UI components to update accordingly.
 */
class ClientManager : public QObject {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_manager";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ClientManager(std::shared_ptr<eventing::service::event_bus> event_bus,
                           QObject* parent = nullptr);
    ~ClientManager() override;

    /**
     * @brief Set the NATS subject prefix used for all outbound messages.
     *
     * Must be called before @c connect(). Format: "ores.{tier}.{instance}",
     * e.g. "ores.dev.local1". Leave empty to use subjects without a prefix.
     */
    void setSubjectPrefix(const std::string& prefix) { subject_prefix_ = prefix; }

    /**
     * @brief Get the current NATS subject prefix.
     */
    const std::string& subjectPrefix() const { return subject_prefix_; }

    /**
     * @brief Connect to the NATS server without logging in.
     */
    LoginResult connect(const std::string& host, std::uint16_t port);

    /**
     * @brief Login on an already connected client.
     */
    LoginResult login(const std::string& username, const std::string& password);

    /**
     * @brief Connect to the server and perform login.
     */
    LoginResult connectAndLogin(
        const std::string& host,
        std::uint16_t port,
        const std::string& username,
        const std::string& password);

    /**
     * @brief Test a connection without affecting main client state.
     */
    LoginResult testConnection(
        const std::string& host,
        std::uint16_t port,
        const std::string& username,
        const std::string& password);

    /**
     * @brief Connect to the server and attempt signup.
     */
    SignupResult signup(
        const std::string& host,
        std::uint16_t port,
        const std::string& username,
        const std::string& email,
        const std::string& password);

    /**
     * @brief Logout the current user and disconnect from the server.
     */
    void disconnect();

    /**
     * @brief Logout the current user without disconnecting.
     */
    bool logout();

    /**
     * @brief Check if currently connected.
     */
    bool isConnected() const;

    /**
     * @deprecated Permission checks are now performed server-side via RBAC.
     */
    [[deprecated("Permission checks are now server-side via RBAC")]]
    bool isAdmin() const { return false; }

    /**
     * @brief Check if currently logged in.
     */
    bool isLoggedIn() const { return session_.is_logged_in(); }

    /**
     * @brief Create a JetStream admin handle for managing streams and consumers.
     *
     * Throws std::runtime_error if not connected.
     */
    [[nodiscard]] nats::service::jetstream_admin admin();

    /**
     * @brief Get the current logged-in user's username.
     */
    std::string currentUsername() const {
        if (!session_.is_logged_in()) return {};
        return session_.auth().username;
    }

    /**
     * @brief Get the current logged-in user's email.
     */
    std::string currentEmail() const { return current_email_; }

    /**
     * @brief Set the current logged-in user's email.
     */
    void setCurrentEmail(const std::string& email) { current_email_ = email; }

    /**
     * @brief Get the account ID if logged in.
     */
    std::optional<boost::uuids::uuid> accountId() const { return current_account_id_; }

    /**
     * @brief Get the server address string.
     */
    std::string serverAddress() const {
        if (!isConnected()) return "";
        return connected_host_ + ":" + std::to_string(connected_port_);
    }

    /**
     * @brief Get the connected server hostname.
     */
    std::string connectedHost() const {
        if (!isConnected()) return "";
        return connected_host_;
    }

    /**
     * @brief Get the connected server port.
     */
    std::uint16_t connectedPort() const {
        if (!isConnected()) return 0;
        return connected_port_;
    }

    /**
     * @brief Get the stored username used for the current session.
     */
    std::string storedUsername() const { return stored_username_; }

    /**
     * @brief Get the stored password used for the current session.
     */
    std::string storedPassword() const { return stored_password_; }

    /**
     * @brief Get the UUID of the currently selected party.
     */
    boost::uuids::uuid currentPartyId() const { return current_party_id_; }

    /**
     * @brief Get the name of the currently selected party.
     */
    QString currentPartyName() const { return current_party_name_; }

    /**
     * @brief Get the category of the currently selected party.
     */
    QString currentPartyCategory() const { return current_party_category_; }

    /**
     * @brief Returns true if the currently selected party is the system party.
     */
    bool isSystemParty() const { return current_party_category_ == "System"; }

    /**
     * @brief Select a party for the current session.
     */
    bool selectParty(const boost::uuids::uuid& party_id, const QString& party_name);

    /**
     * @brief Process a request that does not require authentication.
     *
     * Serializes the request to JSON, sends it via NATS, and deserializes
     * the response.
     *
     * @tparam RequestType Request type (must satisfy nats_request concept)
     * @param request The request to send
     * @return Response on success, error string on failure
     */
    template <nats_request RequestType>
    auto process_request(RequestType request)
        -> std::expected<typename RequestType::response_type, std::string> {
        using ResponseType = typename RequestType::response_type;
        try {
            const auto json_body = rfl::json::write(request);
            auto msg = session_.request(RequestType::nats_subject, json_body);
            const std::string_view data(
                reinterpret_cast<const char*>(msg.data.data()),
                msg.data.size());
            auto result = rfl::json::read<ResponseType>(data);
            if (!result) {
                return std::unexpected(
                    std::string("Failed to deserialize response: ") +
                    result.error().what());
            }
            return std::move(*result);
        } catch (const std::exception& e) {
            return std::unexpected(std::string(e.what()));
        }
    }

    /**
     * @brief Process a request that requires authentication.
     *
     * Checks if logged in before sending. Adds JWT to request headers.
     *
     * @tparam RequestType Request type (must satisfy nats_request concept)
     * @param request The request to send
     * @return Response on success, error string on failure
     */
    template <nats_request RequestType>
    auto process_authenticated_request(RequestType request)
        -> std::expected<typename RequestType::response_type, std::string> {
        using ResponseType = typename RequestType::response_type;
        if (!session_.is_logged_in()) {
            return std::unexpected(std::string("Not logged in"));
        }
        try {
            const auto json_body = rfl::json::write(request);
            auto msg = session_.authenticated_request(
                RequestType::nats_subject, json_body);
            const std::string_view data(
                reinterpret_cast<const char*>(msg.data.data()),
                msg.data.size());
            auto result = rfl::json::read<ResponseType>(data);
            if (!result) {
                return std::unexpected(
                    std::string("Failed to deserialize response: ") +
                    result.error().what());
            }
            return std::move(*result);
        } catch (const std::exception& e) {
            return std::unexpected(std::string(e.what()));
        }
    }

    /**
     * @brief List sessions for an account.
     */
    std::optional<SessionListResult> listSessions(
        const boost::uuids::uuid& accountId,
        std::uint32_t limit = 100,
        std::uint32_t offset = 0);

    /**
     * @brief Get active sessions for the current user.
     */
    std::optional<std::vector<iam::domain::session>> getActiveSessions();

    /**
     * @brief Get time-series samples for a session.
     */
    std::optional<std::vector<iam::messaging::session_sample_dto>>
    getSessionSamples(const boost::uuids::uuid& sessionId);

    // =========================================================================
    // Connection Status Accessors (stubbed for NATS - not all available)
    // =========================================================================

    std::uint64_t bytesSent() const { return 0; }
    std::uint64_t bytesReceived() const { return 0; }
    std::uint64_t lastRttMs() const { return 0; }

    std::optional<std::chrono::steady_clock::time_point> disconnectedSince() const {
        return disconnected_since_;
    }

    // =========================================================================
    // Session Recording (stubbed - not applicable for NATS)
    // =========================================================================

    bool enableRecording(const std::filesystem::path&) { return false; }
    void disableRecording() {}
    bool isRecording() const { return false; }
    std::filesystem::path recordingFilePath() const { return {}; }
    void setRecordingDirectory(const std::filesystem::path& dir) {
        recording_directory_ = dir;
    }
    std::filesystem::path recordingDirectory() const {
        return recording_directory_;
    }

    // =========================================================================
    // Event Subscriptions (stubbed - NATS subscriptions TODO)
    // =========================================================================

    void subscribeToEvent(const std::string&) {}
    void unsubscribeFromEvent(const std::string&) {}

signals:
    void connected();
    void loggedIn();
    void disconnected();
    void reconnecting();
    void reconnected();
    void connectionError(const QString& message);

    void notificationReceived(const QString& eventType, const QDateTime& timestamp,
                              const QStringList& entityIds, const QString& tenantId,
                              int payloadType, const QByteArray& payload);

    void recordingStarted(const QString& filePath);
    void recordingStopped();
    void streamingStarted();
    void streamingStopped();

private:
    // NATS session for connection and authentication
    comms::shell::service::nats_session session_;

    // Subject prefix applied to all outbound NATS messages
    std::string subject_prefix_;

    // Event bus for publishing connection events
    std::shared_ptr<eventing::service::event_bus> event_bus_;

    // Connection details
    std::string connected_host_;
    std::uint16_t connected_port_{0};

    // Session recording directory
    std::filesystem::path recording_directory_;

    // Time point when the connection was lost
    std::optional<std::chrono::steady_clock::time_point> disconnected_since_;

    // Stored credentials for display/reconnect
    std::string stored_username_;
    std::string stored_password_;

    // Current account info (from login)
    std::optional<boost::uuids::uuid> current_account_id_;
    std::string current_email_;

    // Currently selected party context
    boost::uuids::uuid current_party_id_;
    QString current_party_name_;
    QString current_party_category_;
};

}

#endif
