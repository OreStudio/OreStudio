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

#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.iam.api/domain/session.hpp"
#include "ores.iam.api/messaging/session_samples_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/jetstream_admin.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.nats/service/nats_connect_error.hpp"
#include "ores.nats/service/session_expired_error.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.qt/WorkspaceContext.hpp"
#include "ores.qt/export.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <QDateTime>
#include <QObject>
#include <QTimer>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <atomic>
#include <chrono>
#include <concepts>
#include <expected>
#include <filesystem>
#include <memory>
#include <optional>
#include <rfl/json.hpp>
#include <string>
#include <vector>

namespace ores::qt {

// Forward declaration: full definition in IInstrumentFormPopulator.hpp.
// Including the full header here would transitively pull all domain instrument
// headers into every TU that includes ClientManager.hpp.
struct IInstrumentFormPopulator;

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
    QString party_category;       // "System" or "Operational"
    QString business_center_code; // FpML code, e.g. "GBLO", "USNY"

    bool is_system() const {
        return party_category == "System";
    }
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
    bool party_setup_required = false;
    /**
     * @brief Set when the party provisioner wizard has completed but the
     * party is still Inactive. Only meaningful when party_setup_required
     * is false; the client should show this instead of re-launching the
     * wizard.
     */
    QString party_setup_warning;
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
 * @brief Result of a trade list request (metadata only, no instruments).
 */
struct TradeListResult {
    std::vector<trading::domain::trade> trades;
    std::uint32_t total_count = 0;
};

/**
 * @brief Manages the lifecycle of the NATS client and login state.
 *
 * Maintains a persistent NATS connection while allowing the authentication
 * state to be established and torn down repeatedly. Signals changes in
 * connection state to allow UI components to update accordingly.
 */
class ORES_QT_API ClientManager : public QObject {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.client_manager";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    // Standard timeout for quick NATS round-trips (queries, lookups).
    static constexpr std::chrono::seconds fast_timeout{30};
    // Extended timeout for slow server-side operations (provisioning, imports).
    static constexpr std::chrono::seconds slow_timeout{120};

    explicit ClientManager(std::shared_ptr<eventing::service::event_bus> event_bus,
                           QObject* parent = nullptr);
    ~ClientManager() override;

    /**
     * @brief Set the NATS subject prefix used for all outbound messages.
     *
     * Must be called before @c connect(). Format: "ores.{tier}.{instance}",
     * e.g. "ores.dev.local1". Leave empty to use subjects without a prefix.
     */
    void setSubjectPrefix(const std::string& prefix) {
        subject_prefix_ = prefix;
    }

    /**
     * @brief Get the current NATS subject prefix.
     */
    const std::string& subjectPrefix() const {
        return subject_prefix_;
    }

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
    LoginResult connectAndLogin(const std::string& host,
                                std::uint16_t port,
                                const std::string& username,
                                const std::string& password);

    /**
     * @brief Test a connection without affecting main client state.
     */
    LoginResult testConnection(const std::string& host,
                               std::uint16_t port,
                               const std::string& username,
                               const std::string& password);

    /**
     * @brief Connect to the server and attempt signup.
     */
    SignupResult signup(const std::string& host,
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
     *
     * @param timeout  How long to wait for the server acknowledgement.
     *                 Pass a short value (e.g. 3 s) when called from
     *                 disconnect() so the UI does not block on a dead server.
     */
    bool logout(std::chrono::milliseconds timeout = std::chrono::seconds(30));

    /**
     * @brief Check if currently connected.
     */
    bool isConnected() const;

    /**
     * @deprecated Permission checks are now performed server-side via RBAC.
     */
    [[deprecated("Permission checks are now server-side via RBAC")]]
    bool isAdmin() const {
        return false;
    }

    /**
     * @brief Check if currently logged in.
     */
    bool isLoggedIn() const {
        return session_.is_logged_in();
    }

    /**
     * @brief Create a JetStream admin handle for managing streams and consumers.
     *
     * Throws std::runtime_error if not connected.
     */
    [[nodiscard]] nats::service::jetstream_admin admin();

    /**
     * @brief Returns the underlying NATS client for direct subscriptions.
     *
     * Used by ores.marketdata.client (fx_spot_subscription) to subscribe to
     * tick fan-out subjects without routing through ClientManager's
     * entity-change notification mechanism.
     *
     * @pre is_logged_in() must be true.
     * @throws std::runtime_error if not logged in.
     */
    [[nodiscard]] nats::service::client& nats_client();

    /**
     * @brief Get the current logged-in user's username.
     */
    std::string currentUsername() const {
        if (!session_.is_logged_in())
            return {};
        return session_.auth().username;
    }

    /**
     * @brief Get the current logged-in user's email.
     */
    std::string currentEmail() const {
        return current_email_;
    }

    /**
     * @brief Set the current logged-in user's email.
     */
    void setCurrentEmail(const std::string& email) {
        current_email_ = email;
    }

    /**
     * @brief Get the account ID if logged in.
     */
    std::optional<boost::uuids::uuid> accountId() const {
        return current_account_id_;
    }

    /**
     * @brief Get the server address string.
     */
    std::string serverAddress() const {
        if (!isConnected())
            return "";
        return connected_host_ + ":" + std::to_string(connected_port_);
    }

    /**
     * @brief Get the connected server hostname.
     */
    std::string connectedHost() const {
        if (!isConnected())
            return "";
        return connected_host_;
    }

    /**
     * @brief Get the connected server port.
     */
    std::uint16_t connectedPort() const {
        if (!isConnected())
            return 0;
        return connected_port_;
    }

    /**
     * @brief Get the stored username used for the current session.
     */
    std::string storedUsername() const {
        return stored_username_;
    }

    /**
     * @brief Get the stored password used for the current session.
     */
    std::string storedPassword() const {
        return stored_password_;
    }

    /**
     * @brief Get the UUID of the currently selected party.
     */
    boost::uuids::uuid currentPartyId() const {
        return current_party_id_;
    }

    /**
     * @brief Get the tenant_id for the current session (empty if not logged in).
     */
    std::string currentTenantId() const {
        if (!session_.is_logged_in())
            return {};
        return session_.auth().tenant_id;
    }

    /**
     * @brief Get the name of the currently selected party.
     */
    QString currentPartyName() const {
        return current_party_name_;
    }

    /**
     * @brief Get the category of the currently selected party.
     */
    QString currentPartyCategory() const {
        return current_party_category_;
    }

    /**
     * @brief Returns true if the currently selected party is the system party.
     */
    bool isSystemParty() const {
        return current_party_category_ == "System";
    }

    /**
     * @brief Whether the last selectParty call indicated the party needs setup.
     */
    bool lastPartySetupRequired() const {
        return last_party_setup_required_;
    }

    /**
     * @brief Warning from the last selectParty/login call when the party
     * provisioner wizard has completed but the party is still Inactive.
     * Empty when there is nothing to show.
     */
    const QString& lastPartySetupWarning() const {
        return last_party_setup_warning_;
    }

    /**
     * @brief HTTP base URL discovered during login via NATS service discovery.
     *
     * Populated by login() on success. Empty between disconnect() and the next
     * successful login.
     */
    const std::string& httpBaseUrl() const {
        return http_base_url_;
    }

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
            const std::string_view data(reinterpret_cast<const char*>(msg.data.data()),
                                        msg.data.size());
            auto result = rfl::json::read<ResponseType>(data);
            if (!result) {
                return std::unexpected(std::string("Failed to deserialize response: ") +
                                       result.error().what());
            }
            return std::move(*result);
        } catch (const ores::nats::service::nats_connect_error&) {
            throw; // Propagate so connect() can map to a user-visible message
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
     * @param timeout  Request timeout (default: 30 seconds)
     * @return Response on success, error string on failure
     */
    template <nats_request RequestType>
    auto process_authenticated_request(RequestType request,
                                       std::chrono::milliseconds timeout = std::chrono::seconds(30))
        -> std::expected<typename RequestType::response_type, std::string> {
        using ResponseType = typename RequestType::response_type;
        try {
            const auto raw = send_authenticated_request(
                RequestType::nats_subject, rfl::json::write(request), timeout);
            auto result = rfl::json::read<ResponseType>(raw);
            if (!result) {
                return std::unexpected(std::string("Failed to deserialize response: ") +
                                       result.error().what());
            }
            return std::move(*result);
        } catch (const ores::nats::service::nats_connect_error&) {
            throw; // Propagate so connect() can map to a user-visible message
        } catch (const ores::nats::service::session_expired_error& e) {
            using namespace ores::logging;
            BOOST_LOG_SEV(lg(), warn) << "Session expired: " << e.what();
            QMetaObject::invokeMethod(this, "sessionExpired", Qt::QueuedConnection);
            return std::unexpected(std::string(e.what()));
        } catch (const std::exception& e) {
            return std::unexpected(std::string(e.what()));
        }
    }

    /**
     * @brief Export all trades and instruments under a taxonomy node.
     *
     * Concrete (non-template) wrapper around process_authenticated_request for
     * export_portfolio_request. Implemented in ClientManagerExportPortfolio.cpp
     * so that rfl::json::read<export_portfolio_response> is instantiated in a
     * dedicated translation unit — avoiding MSVC C1202 in large callers such as
     * BookMdiWindow.cpp whose accumulated template context would otherwise be
     * pushed over the limit by trade_instrument's rfl field-name Literals.
     */
    std::expected<trading::messaging::export_portfolio_response, std::string>
    exportPortfolio(trading::messaging::export_portfolio_request request,
                    std::chrono::milliseconds timeout = std::chrono::seconds(30));

    /**
     * @brief List sessions for an account.
     */
    std::optional<SessionListResult> listSessions(const boost::uuids::uuid& accountId,
                                                  std::uint32_t limit = 100,
                                                  std::uint32_t offset = 0);

    /**
     * @brief List trades under a taxonomy node.
     *
     * @p node_id may be a book, portfolio, or business unit id; the server
     * resolves it to the book-id subtree. Omit @p node_id to list all trades
     * visible to the tenant.
     */
    std::optional<TradeListResult>
    listTrades(std::optional<boost::uuids::uuid> node_id = std::nullopt,
               std::uint32_t offset = 0,
               std::uint32_t limit = 100);

    /**
     * @brief Fetch a single trade and dispatch its instrument to the populator.
     *
     * Phase 1 reads the envelope (success, message, trade). Phase 2 dispatches
     * on (product_type, trade_type) and calls the matching populate() overload
     * on the populator directly — no trade_instrument variant is constructed.
     *
     * @param trade_id  UUID string of the trade.
     * @param populator Receives the concrete instrument via the matching overload.
     * @return The trade on success, nullopt on failure or not found.
     */
    std::optional<trading::domain::trade> getTradeInstrument(const std::string& trade_id,
                                                             IInstrumentFormPopulator& populator);

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

    std::uint64_t bytesSent() const {
        return 0;
    }
    std::uint64_t bytesReceived() const {
        return 0;
    }
    std::uint64_t lastRttMs() const {
        return 0;
    }

    std::optional<std::chrono::steady_clock::time_point> disconnectedSince() const {
        return disconnected_since_;
    }

    // =========================================================================
    // Session Recording (stubbed - not applicable for NATS)
    // =========================================================================

    bool enableRecording(const std::filesystem::path&) {
        return false;
    }
    void disableRecording() {}
    bool isRecording() const {
        return false;
    }
    std::filesystem::path recordingFilePath() const {
        return {};
    }
    void setRecordingDirectory(const std::filesystem::path& dir) {
        recording_directory_ = dir;
    }
    std::filesystem::path recordingDirectory() const {
        return recording_directory_;
    }

    // =========================================================================
    // Event Subscriptions
    // =========================================================================

    void subscribeToEvent(const std::string& subject);
    void unsubscribeFromEvent(const std::string& subject);

    /**
     * @brief Update the active workspace context.
     *
     * Called by plugin controllers when the MDI area emits a workspace change.
     * All subsequent authenticated_request calls will forward the new workspace
     * id as X-Workspace-Id.
     */
    void setWorkspaceContext(const WorkspaceContext& ctx) {
        workspace_context_ = ctx;
    }

    /**
     * @brief Get the current application-level workspace context.
     */
    const WorkspaceContext& workspaceContext() const {
        return workspace_context_;
    }

    /**
     * @brief Process an authenticated request with an explicit workspace override.
     *
     * Like process_authenticated_request(request) but uses workspace_id instead
     * of the shared workspace_context_. Used by per-window workspace selectors so
     * each window can query a different workspace simultaneously.
     */
    template <nats_request RequestType>
    auto process_authenticated_request(RequestType request,
                                       const std::string& workspace_id_override,
                                       std::chrono::milliseconds timeout = fast_timeout)
        -> std::expected<typename RequestType::response_type, std::string> {
        using ResponseType = typename RequestType::response_type;
        try {
            const auto raw = send_authenticated_request_with_workspace(RequestType::nats_subject,
                                                                       rfl::json::write(request),
                                                                       workspace_id_override,
                                                                       timeout);
            auto result = rfl::json::read<ResponseType>(raw);
            if (!result) {
                return std::unexpected(std::string("Failed to deserialize response: ") +
                                       result.error().what());
            }
            return std::move(*result);
        } catch (const ores::nats::service::nats_connect_error&) {
            throw;
        } catch (const ores::nats::service::session_expired_error& e) {
            using namespace ores::logging;
            BOOST_LOG_SEV(lg(), warn) << "Session expired: " << e.what();
            QMetaObject::invokeMethod(this, "sessionExpired", Qt::QueuedConnection);
            return std::unexpected(std::string(e.what()));
        } catch (const std::exception& e) {
            return std::unexpected(std::string(e.what()));
        }
    }

    /**
     * @brief Process an authenticated request with a full workspace context.
     *
     * Like the workspace_id_override overload but also forwards the resolution
     * chain as X-Workspace-Resolution so the server returns inherited definitions
     * from ancestor workspaces as well as the selected workspace's own.
     */
    template <nats_request RequestType>
    auto process_authenticated_request(RequestType request,
                                       const WorkspaceContext& workspace_ctx,
                                       std::chrono::milliseconds timeout = fast_timeout)
        -> std::expected<typename RequestType::response_type, std::string> {
        using ResponseType = typename RequestType::response_type;
        try {
            std::vector<std::string> chain;
            for (const auto& wid : workspace_ctx.resolution_order)
                chain.push_back(wid.toStdString());
            const auto raw =
                send_authenticated_request_with_workspace_ctx(RequestType::nats_subject,
                                                              rfl::json::write(request),
                                                              workspace_ctx.id.toStdString(),
                                                              std::move(chain),
                                                              timeout);
            auto result = rfl::json::read<ResponseType>(raw);
            if (!result) {
                return std::unexpected(std::string("Failed to deserialize response: ") +
                                       result.error().what());
            }
            return std::move(*result);
        } catch (const ores::nats::service::nats_connect_error&) {
            throw;
        } catch (const ores::nats::service::session_expired_error& e) {
            using namespace ores::logging;
            BOOST_LOG_SEV(lg(), warn) << "Session expired: " << e.what();
            QMetaObject::invokeMethod(this, "sessionExpired", Qt::QueuedConnection);
            return std::unexpected(std::string(e.what()));
        } catch (const std::exception& e) {
            return std::unexpected(std::string(e.what()));
        }
    }

signals:
    void connected();
    void loggedIn();
    void disconnected();
    void reconnecting();
    void reconnected();
    void connectionError(const QString& message);

    /**
     * @brief Emitted when the session can no longer be refreshed.
     *
     * Fired either by the proactive refresh timer (max_session_exceeded) or
     * reactively when authenticated_request() throws after a failed refresh.
     * Connect to this signal to show the session-expired dialog and re-login.
     */
    void sessionExpired();

    void notificationReceived(const QString& eventType,
                              const QDateTime& timestamp,
                              const QStringList& entityIds,
                              const QString& tenantId,
                              int payloadType,
                              const QByteArray& payload);

    void recordingStarted(const QString& filePath);
    void recordingStopped();
    void streamingStarted();
    void streamingStopped();

private:
    // Fraction of token lifetime at which the proactive refresh fires
    static constexpr double refresh_lifetime_ratio = 0.8;

    /**
     * @brief Start/restart the proactive refresh timer.
     *
     * Fires at refresh_lifetime_ratio of @p lifetime_s seconds so the token
     * is refreshed before it expires. Call after every successful login or
     * party selection.
     */
    void arm_refresh_timer(int lifetime_s);

    /**
     * @brief Called by refresh_timer_; sends iam.v1.auth.refresh.
     *
     * Updates the stored JWT on success and re-arms the timer.
     * Emits sessionExpired() if the server returns max_session_exceeded.
     */
    void onRefreshTimer();

    /**
     * @brief Send an authenticated NATS request and return the raw JSON response.
     *
     * Handles auth check, correlation ID, and session scoping. Throws on any
     * error; does not deserialize — that is the caller's responsibility.
     */
    std::string send_authenticated_request(std::string_view subject,
                                           std::string json_body,
                                           std::chrono::milliseconds timeout);

    /**
     * @brief Like send_authenticated_request but with an explicit workspace id.
     *
     * Used by the per-workspace overload of process_authenticated_request so
     * per-window requests do not mutate the shared workspace_context_.
     */
    std::string send_authenticated_request_with_workspace(std::string_view subject,
                                                          std::string json_body,
                                                          const std::string& workspace_id,
                                                          std::chrono::milliseconds timeout);

    /**
     * @brief Like send_authenticated_request_with_workspace but also forwards
     * the workspace resolution chain as X-Workspace-Resolution.
     */
    std::string
    send_authenticated_request_with_workspace_ctx(std::string_view subject,
                                                  std::string json_body,
                                                  const std::string& workspace_id,
                                                  std::vector<std::string> resolution_chain,
                                                  std::chrono::milliseconds timeout);

    // NATS session for connection and authentication
    ores::nats::service::nats_client session_;

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

    // Session trace ID generated on login, forwarded on every NATS request
    // as Nats-Session-Id to group all calls from this login session in logs.
    std::string session_id_;

    // Active workspace context; forwarded as X-Workspace-Id on every request.
    WorkspaceContext workspace_context_;

    // Currently selected party context
    boost::uuids::uuid current_party_id_;
    QString current_party_name_;
    QString current_party_category_;
    bool last_party_setup_required_ = false;
    QString last_party_setup_warning_;

    // HTTP server base URL discovered post-login via NATS.
    std::string http_base_url_;

    // Active NATS event subscriptions keyed by subject
    std::unordered_map<std::string, nats::service::subscription> nats_subscriptions_;

    // Proactive token refresh timer (fires before token expires)
    QTimer* refresh_timer_ = nullptr;
};

}

#endif
