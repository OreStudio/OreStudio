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

#include <memory>
#include <optional>
#include <expected>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/error_protocol.hpp"
#include "ores.comms/net/client.hpp"
#include "ores.comms/net/client_options.hpp"

namespace ores::comms::net {

/**
 * @brief Information about the client's authenticated session.
 */
struct client_session_info {
    boost::uuids::uuid account_id;
    std::string username;
    bool is_admin;
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
    server_error
};

template<typename Request>
concept Serializable = requires(Request req) {
    { req.serialize() } -> std::convertible_to<std::vector<std::byte>>;
};

template<typename Response>
concept Deserializable = requires(std::span<const std::byte> data) {
    {
        Response::deserialize(data)
    } -> std::same_as<std::expected<Response, messaging::error_code>>;
};

/**
 * @brief Client-side session manager providing auth-aware request handling.
 *
 * This class wraps a net::client and provides:
 * - Connection lifecycle management
 * - Authentication state tracking
 * - Auth-aware request processing (checks login state before sending)
 *
 * Designed to be shared between ores.shell and ores.qt clients.
 */
class client_session final {
private:
    inline static std::string_view logger_name =
        "ores.comms.net.client_session";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    client_session() = default;
    ~client_session();

    // Non-copyable
    client_session(const client_session&) = delete;
    client_session& operator=(const client_session&) = delete;

    // Movable
    client_session(client_session&&) = default;
    client_session& operator=(client_session&&) = default;

    /**
     * @brief Connect to the server.
     *
     * @param options Connection options
     * @return Empty expected on success, error on failure
     */
    std::expected<void, client_session_error> connect(client_options options);

    /**
     * @brief Disconnect from the server.
     *
     * Will logout first if logged in.
     */
    void disconnect();

    /**
     * @brief Check if connected to server.
     */
    [[nodiscard]] bool is_connected() const noexcept;

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
     * @brief Check if logged in as admin.
     */
    [[nodiscard]] bool is_admin() const noexcept {
        return session_info_.has_value() && session_info_->is_admin;
    }

    /**
     * @brief Get current session info if logged in.
     */
    [[nodiscard]] const std::optional<client_session_info>& session_info() const noexcept {
        return session_info_;
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
    std::expected<ResponseType, client_session_error>
    process_request(RequestType request) {
        using namespace ores::utility::log;

        if (!client_ || !client_->is_connected()) {
            BOOST_LOG_SEV(lg(), error) << "Not connected to server";
            return std::unexpected(client_session_error::not_connected);
        }

        BOOST_LOG_SEV(lg(), debug) << "Processing request type: "
                                   << static_cast<int>(RequestMsgType);

        auto payload = request.serialize();
        messaging::frame request_frame(RequestMsgType, 0, std::move(payload));

        auto response_result = client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Request failed with error code: "
                                       << static_cast<int>(response_result.error());
            return std::unexpected(client_session_error::request_failed);
        }

        // Check for error response
        if (response_result->header().type == messaging::message_type::error_response) {
            auto err_resp = messaging::error_response::deserialize(
                response_result->payload());
            if (err_resp) {
                BOOST_LOG_SEV(lg(), error) << "Server returned error: "
                                           << err_resp->message;
            }
            return std::unexpected(client_session_error::server_error);
        }

        auto response = ResponseType::deserialize(response_result->payload());
        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
            return std::unexpected(client_session_error::deserialization_failed);
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
    std::expected<ResponseType, client_session_error>
    process_authenticated_request(RequestType request) {
        using namespace ores::utility::log;
        if (!is_logged_in()) {
            BOOST_LOG_SEV(lg(), warn) << "Attempted authenticated request while "
                                      << "not logged in";
            return std::unexpected(client_session_error::not_logged_in);
        }
        return process_request<RequestType, ResponseType, RequestMsgType>(
            std::move(request));
    }

    /**
     * @brief Process a request that requires admin privileges.
     *
     * Checks if logged in as admin before sending.
     *
     * @tparam RequestType Request message type (must be Serializable)
     * @tparam ResponseType Response message type (must be Deserializable)
     * @tparam RequestMsgType The message_type enum value for the request
     * @param request The request to send
     * @return Response on success, error on failure (including not_logged_in,
     *         admin_required)
     */
    template <Serializable RequestType,
              Deserializable ResponseType,
              messaging::message_type RequestMsgType>
    std::expected<ResponseType, client_session_error>
    process_admin_request(RequestType request) {
        using namespace ores::utility::log;
        if (!is_logged_in()) {
            BOOST_LOG_SEV(lg(), warn) << "Attempted admin request while "
                                      << "not logged in";
            return std::unexpected(client_session_error::not_logged_in);
        }
        if (!is_admin()) {
            BOOST_LOG_SEV(lg(), warn) << "Attempted admin request without "
                                      << "admin privileges";
            return std::unexpected(client_session_error::admin_required);
        }
        return process_request<RequestType, ResponseType, RequestMsgType>(
            std::move(request));
    }

private:
    std::shared_ptr<client> client_;
    std::optional<client_session_info> session_info_;
};

/**
 * @brief Convert client_session_error to string for display.
 */
std::string to_string(client_session_error error);

}

#endif
