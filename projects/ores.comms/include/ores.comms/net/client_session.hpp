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
#ifndef ORES_COMMS_NET_CLIENT_SESSION_HPP
#define ORES_COMMS_NET_CLIENT_SESSION_HPP

#include <mutex>
#include <deque>
#include <memory>
#include <chrono>
#include <optional>
#include <expected>
#include <functional>
#include <string>
#include <vector>
#include <set>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/error_protocol.hpp"
#include "ores.comms/net/client.hpp"
#include "ores.comms/net/client_options.hpp"

namespace ores::comms::service {

class remote_event_adapter;

}

namespace ores::comms::net {

/**
 * @brief Information about the client's authenticated session.
 *
 * Note: Permission checks are now handled server-side via RBAC.
 * The client only tracks basic session info.
 */
struct client_session_info {
    boost::uuids::uuid account_id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    std::string username;
    std::string email;
};

/**
 * @brief A notification received from the server.
 *
 * Stored in a queue for display at the next prompt.
 */
struct pending_notification {
    std::string event_type;
    std::chrono::system_clock::time_point timestamp;
    std::vector<std::string> entity_ids;
    std::string tenant_id;
    messaging::payload_type pt{messaging::payload_type::none};
    std::optional<std::vector<std::byte>> payload;
};

/**
 * @brief Error codes specific to client session operations.
 */
enum class client_session_error {
    not_connected,
    not_logged_in,
    login_required,
    admin_required,
    request_failed,
    deserialization_failed,
    server_error,
    connection_lost
};

/**
 * @brief Error information returned from client session operations.
 *
 * Contains both an error code and an optional detailed message from the server.
 */
struct session_error {
    client_session_error code;
    std::string message;

    explicit session_error(client_session_error c)
        : code(c) {}

    session_error(client_session_error c, std::string msg)
        : code(c), message(std::move(msg)) {}
};

template<typename Request>
concept Serializable = requires(Request req) {
    { req.serialize() } -> std::convertible_to<std::vector<std::byte>>;
};

template<typename Response>
concept Deserializable = requires(std::span<const std::byte> data) {
    {
        Response::deserialize(data)
    } -> std::same_as<std::expected<Response, ores::utility::serialization::error_code>>;
};

/**
 * @brief Client-side session manager providing auth-aware request handling.
 *
 * This class wraps a net::client and provides:
 * - Connection lifecycle management
 * - Authentication state tracking
 * - Auth-aware request processing (checks login state before sending)
 *
 * Designed to be shared between ores.comms.shell and ores.qt clients.
 */
class client_session final {
private:
    inline static std::string_view logger_name =
        "ores.comms.net.client_session";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    client_session();
    ~client_session();

    // Non-copyable, non-movable (contains mutex)
    client_session(const client_session&) = delete;
    client_session& operator=(const client_session&) = delete;
    client_session(client_session&&) = delete;
    client_session& operator=(client_session&&) = delete;

    /**
     * @brief Connect to the server.
     *
     * Creates a new client internally and connects. Use this for standalone
     * usage (e.g., in ores.comms.shell).
     *
     * @param options Connection options
     * @return Empty expected on success, error on failure
     */
    std::expected<void, session_error> connect(client_options options);

    /**
     * @brief Attach an external client to this session.
     *
     * Use this when the client lifecycle is managed externally (e.g., in
     * ores.qt where ClientManager owns the client). The session will use
     * the provided client for requests but will not manage its lifecycle.
     *
     * @param external_client The client to attach (must be connected)
     * @return Empty expected on success, error on failure
     */
    std::expected<void, session_error>
    attach_client(std::shared_ptr<client> external_client);

    /**
     * @brief Disconnect from the server.
     *
     * For internally-created clients: disconnects and destroys the client.
     * For externally-attached clients: detaches without disconnecting
     * (caller is responsible for client lifecycle).
     */
    void disconnect();

    /**
     * @brief Detach an externally-attached client.
     *
     * Clears session state and event adapter, but does not disconnect the
     * client. Use this when transferring client ownership or when the
     * external client will be reused.
     */
    void detach_client();

    /**
     * @brief Check if connected to server.
     */
    [[nodiscard]] bool is_connected() const noexcept;

    /**
     * @brief Get the underlying client.
     *
     * Provides access to the client for advanced use cases like telemetry
     * streaming. Returns nullptr if not connected.
     */
    [[nodiscard]] std::shared_ptr<client> get_client() const noexcept {
        return client_;
    }

    /**
     * @brief Disable auto-reconnect on the underlying client.
     *
     * Call this before logout to prevent the client from attempting
     * to reconnect when the server closes the connection.
     */
    void disable_auto_reconnect() {
        if (client_) {
            client_->disable_auto_reconnect();
        }
    }

    /**
     * @brief Check if logged in.
     */
    [[nodiscard]] bool is_logged_in() const noexcept {
        return session_info_.has_value();
    }

    /**
     * @brief Set session info after successful login.
     *
     * Called by the application layer after processing a login response.
     *
     * @param info Session information from login response
     */
    void set_session_info(client_session_info info) {
        session_info_ = std::move(info);
    }

    /**
     * @brief Clear session info on logout.
     *
     * Called by the application layer after logout or disconnect.
     */
    void clear_session_info() noexcept {
        session_info_.reset();
    }

    /**
     * @brief Get current session info if logged in.
     */
    [[nodiscard]] const std::optional<client_session_info>& session_info() const noexcept {
        return session_info_;
    }

    /**
     * @brief Get the current username if logged in.
     */
    [[nodiscard]] std::string username() const noexcept {
        return session_info_.has_value() ? session_info_->username : std::string{};
    }

    /**
     * @brief Get the current email if logged in.
     */
    [[nodiscard]] std::string email() const noexcept {
        return session_info_.has_value() ? session_info_->email : std::string{};
    }

    /**
     * @brief Set the current email.
     *
     * Used after successful email update to keep local state in sync.
     */
    void set_email(const std::string& email) {
        if (session_info_.has_value()) {
            session_info_->email = email;
        }
    }

    /**
     * @brief Get the account ID if logged in.
     */
    [[nodiscard]] std::optional<boost::uuids::uuid> account_id() const noexcept {
        if (session_info_.has_value()) {
            return session_info_->account_id;
        }
        return std::nullopt;
    }

    /**
     * @brief Process a request that does not require authentication.
     *
     * Use this for messages like bootstrap_status_request that don't need login.
     *
     * @tparam RequestType Request message type (must be Serializable)
     * @tparam ResponseType Response message type (must be Deserializable)
     * @tparam RequestMsgType The message_type enum value for the request
     * @param request The request to send
     * @return Response on success, error on failure
     */
    template <Serializable RequestType,
              Deserializable ResponseType,
              messaging::message_type RequestMsgType>
    std::expected<ResponseType, session_error>
    process_request(RequestType request) {
        using namespace ores::logging;

        if (!client_ || !client_->is_connected()) {
            BOOST_LOG_SEV(lg(), error) << "Not connected to server";
            return std::unexpected(session_error(client_session_error::not_connected));
        }

        BOOST_LOG_SEV(lg(), debug) << "Processing request type: "
                                   << RequestMsgType;

        auto payload = request.serialize();
        messaging::frame request_frame(RequestMsgType, 0, std::move(payload));

        auto response_result = client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            auto error_code = response_result.error();
            BOOST_LOG_SEV(lg(), error) << "Request failed with error code: "
                                       << static_cast<int>(error_code);
            // Check if this is a network error indicating connection loss
            if (error_code == ores::utility::serialization::error_code::network_error) {
                return std::unexpected(session_error(
                    client_session_error::connection_lost,
                    "Connection to server lost"));
            }
            return std::unexpected(session_error(
                client_session_error::request_failed,
                "Request failed: " + ores::utility::serialization::to_string(error_code)));
        }

        auto decompressed = response_result->decompressed_payload();
        if (!decompressed) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress response payload";
            return std::unexpected(session_error(client_session_error::deserialization_failed));
        }

        // Check for error response
        if (response_result->header().type == messaging::message_type::error_response) {
            auto err_resp = messaging::error_response::deserialize(*decompressed);
            if (err_resp) {
                BOOST_LOG_SEV(lg(), error) << "Server returned error: "
                                           << err_resp->message;
                return std::unexpected(session_error(
                    client_session_error::server_error,
                    err_resp->message));
            }
            return std::unexpected(session_error(client_session_error::server_error));
        }

        auto response = ResponseType::deserialize(*decompressed);
        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
            return std::unexpected(session_error(client_session_error::deserialization_failed));
        }

        BOOST_LOG_SEV(lg(), debug) << "Successfully processed request";
        return std::move(*response);
    }

    /**
     * @brief Process a request that requires authentication.
     *
     * Checks if logged in before sending. Use this for most requests.
     *
     * @tparam RequestType Request message type (must be Serializable)
     * @tparam ResponseType Response message type (must be Deserializable)
     * @tparam RequestMsgType The message_type enum value for the request
     * @param request The request to send
     * @return Response on success, error on failure (including not_logged_in)
     */
    template <Serializable RequestType,
              Deserializable ResponseType,
              messaging::message_type RequestMsgType>
    std::expected<ResponseType, session_error>
    process_authenticated_request(RequestType request) {
        using namespace ores::logging;
        if (!is_logged_in()) {
            BOOST_LOG_SEV(lg(), warn) << "Attempted authenticated request while "
                                      << "not logged in";
            return std::unexpected(session_error(client_session_error::not_logged_in));
        }
        return process_request<RequestType, ResponseType, RequestMsgType>(
            std::move(request));
    }

    /**
     * @brief Process a request that requires admin privileges.
     *
     * @deprecated Permission checks are now performed server-side via RBAC.
     *             Use process_authenticated_request instead; the server will
     *             return a permission denied error if the user lacks the
     *             required permissions.
     *
     * @tparam RequestType Request message type (must be Serializable)
     * @tparam ResponseType Response message type (must be Deserializable)
     * @tparam RequestMsgType The message_type enum value for the request
     * @param request The request to send
     * @return Response on success, error on failure (including not_logged_in)
     */
    template <Serializable RequestType,
              Deserializable ResponseType,
              messaging::message_type RequestMsgType>
    [[deprecated("Permission checks are now server-side via RBAC")]]
    std::expected<ResponseType, session_error>
    process_admin_request(RequestType request) {
        // Permission checks now happen server-side via RBAC
        return process_authenticated_request<RequestType, ResponseType, RequestMsgType>(
            std::move(request));
    }

    // =========================================================================
    // Traits-based process_request overloads
    // =========================================================================
    // These overloads use message_traits to infer the response type and
    // message_type enum from the request type, simplifying the API.

    /**
     * @brief Process a request using message_traits (does not require auth).
     *
     * Uses message_traits to automatically determine the response type and
     * message_type enum value from the request type.
     *
     * @tparam RequestType Request message type (must have message_traits)
     * @param request The request to send
     * @return Response on success, error on failure
     */
    template <typename RequestType>
        requires messaging::has_message_traits<RequestType>
    std::expected<typename messaging::message_traits<RequestType>::response_type,
                  session_error>
    process_request(RequestType request) {
        using traits = messaging::message_traits<RequestType>;
        return process_request<
            RequestType,
            typename traits::response_type,
            traits::request_message_type>(std::move(request));
    }

    /**
     * @brief Process a request using message_traits (requires authentication).
     *
     * @tparam RequestType Request message type (must have message_traits)
     * @param request The request to send
     * @return Response on success, error on failure
     */
    template <typename RequestType>
        requires messaging::has_message_traits<RequestType>
    std::expected<typename messaging::message_traits<RequestType>::response_type,
                  session_error>
    process_authenticated_request(RequestType request) {
        using traits = messaging::message_traits<RequestType>;
        return process_authenticated_request<
            RequestType,
            typename traits::response_type,
            traits::request_message_type>(std::move(request));
    }

    /**
     * @brief Process a request using message_traits (requires admin).
     *
     * @deprecated Permission checks are now performed server-side via RBAC.
     *             Use process_authenticated_request instead.
     *
     * @tparam RequestType Request message type (must have message_traits)
     * @param request The request to send
     * @return Response on success, error on failure
     */
    template <typename RequestType>
        requires messaging::has_message_traits<RequestType>
    [[deprecated("Permission checks are now server-side via RBAC")]]
    std::expected<typename messaging::message_traits<RequestType>::response_type,
                  session_error>
    process_admin_request(RequestType request) {
        // Permission checks now happen server-side via RBAC
        return process_authenticated_request(std::move(request));
    }

    /**
     * @brief Subscribe to notifications for an event type.
     *
     * Delegates to remote_event_adapter to send a SUBSCRIBE protocol message.
     * Received notifications are queued for retrieval via take_pending_notifications().
     *
     * @param event_type The fully qualified event type name (e.g., "ores.refdata.currency_changed")
     * @return True if subscription succeeded, false otherwise
     */
    bool subscribe(const std::string& event_type);

    /**
     * @brief Unsubscribe from notifications for an event type.
     *
     * Delegates to remote_event_adapter to send an UNSUBSCRIBE protocol message.
     *
     * @param event_type The fully qualified event type name
     * @return True if unsubscription succeeded, false otherwise
     */
    bool unsubscribe(const std::string& event_type);

    /**
     * @brief Check if currently subscribed to an event type.
     *
     * @param event_type The event type to check
     * @return True if subscribed, false otherwise
     */
    [[nodiscard]] bool is_subscribed(const std::string& event_type) const;

    /**
     * @brief Get the set of currently subscribed event types.
     *
     * @return Set of event type names
     */
    [[nodiscard]] std::set<std::string> get_subscriptions() const;

    /**
     * @brief Get all pending notifications and clear the queue.
     *
     * Returns notifications in the order they were received. The internal
     * queue is cleared after this call.
     *
     * @return Vector of pending notifications
     */
    std::vector<pending_notification> take_pending_notifications();

    /**
     * @brief Check if there are any pending notifications.
     *
     * @return True if there are pending notifications
     */
    [[nodiscard]] bool has_pending_notifications() const;

    /**
     * @brief Notification callback function type.
     */
    using notification_callback_t = std::function<void(
        const std::string& event_type,
        std::chrono::system_clock::time_point timestamp,
        const std::vector<std::string>& entity_ids,
        const std::string& tenant_id,
        messaging::payload_type pt,
        const std::optional<std::vector<std::byte>>& payload)>;

    /**
     * @brief Set an external notification callback.
     *
     * When set, notifications will be delivered to this callback instead of
     * being queued to pending_notifications_. This is useful for GUI clients
     * that need to emit signals on notification receipt.
     *
     * @param callback The callback to invoke on notification, or nullptr to
     *                 revert to internal queuing
     */
    void set_notification_callback(notification_callback_t callback);

private:
    /**
     * @brief Handle incoming notification from the adapter.
     *
     * Called by the notification callback registered on the remote_event_adapter.
     * Queues notifications for later retrieval.
     */
    void on_notification(const std::string& event_type,
        std::chrono::system_clock::time_point timestamp,
        const std::vector<std::string>& entity_ids,
        const std::string& tenant_id,
        messaging::payload_type pt,
        const std::optional<std::vector<std::byte>>& payload);

    std::shared_ptr<client> client_;
    std::unique_ptr<service::remote_event_adapter> event_adapter_;
    std::optional<client_session_info> session_info_;
    mutable std::mutex notifications_mutex_;
    std::deque<pending_notification> pending_notifications_;

    // External notification callback (if set, replaces internal queuing)
    notification_callback_t external_notification_callback_;

    // True when client is externally managed (attached via attach_client).
    // In this mode, disconnect() will not call client_->disconnect().
    bool external_client_{false};
};

/**
 * @brief Convert client_session_error to string for display.
 */
std::string to_string(client_session_error error);

/**
 * @brief Convert session_error to string for display.
 *
 * Returns the detailed message if available, otherwise falls back to the
 * generic error code message.
 */
std::string to_string(const session_error& error);

}

#endif
