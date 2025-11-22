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
#ifndef ORES_SHELL_APP_CLIENT_MANAGER_HPP
#define ORES_SHELL_APP_CLIENT_MANAGER_HPP

#include <memory>
#include <optional>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/protocol/handshake.hpp"
#include "ores.comms/net/client.hpp"
#include "ores.shell/config/login_options.hpp"

namespace ores::shell::app {

template<typename Request>
concept Serialialisable = requires(Request req) {
    { req.serialize() } -> std::convertible_to<std::vector<std::byte>>;
};

template<typename Response>
concept Deserialisable = requires(std::span<const std::byte> data) {
    {
        Response::deserialize(data)
    } -> std::same_as<std::expected<Response, comms::protocol::error_code>>;
};


class client_manager final {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.shell.app.client_manager");
        return instance;
    }

    void connect(comms::net::client_options config);

    /**
     * @brief Attempt to connect automatically if connection options are provided.
     *
     * Performs connection automatically based on provided configuration.
     */
    bool auto_connect();

    /**
     * @brief Attempt to login automatically if login options are provided.
     *
     * Performs login automatically based on provided configuration after connecting.
     */
    bool auto_login();

public:
    client_manager(std::ostream& out,
        std::optional<comms::net::client_options> connection_config = std::nullopt,
        std::optional<config::login_options> login_config = std::nullopt);

    ~client_manager();

    template <Serialialisable RequestType,
              Deserialisable ResponseType,
              comms::protocol::message_type RequestMsgType>
    std::optional<ResponseType> process_request(RequestType request) {
        using namespace ores::utility::log;
        if (!client_ || !client_->is_connected()) {
            output_ << "✗ Not connected to server. Use 'connect' command first"
                    << std::endl;
            return std::nullopt;
        }

        BOOST_LOG_SEV(lg(), debug) << "Initiating request.";
        auto payload = request.serialize();

        comms::protocol::frame request_frame(RequestMsgType, 0, std::move(payload));
        BOOST_LOG_SEV(lg(), debug) << "Sending request frame";

        auto response_result = client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Request failed with error code: "
                                       << static_cast<int>(response_result.error());
            output_ << "✗ Request failed" << std::endl;
            return std::nullopt;
        }

        using comms::protocol::message_type;
        if (response_result->header().type == message_type::error_response) {
            using comms::protocol::error_response;
            auto err_resp = error_response::deserialize(response_result->payload());
            if (err_resp) {
                BOOST_LOG_SEV(lg(), error) << "Server returned error: "
                                           << err_resp->message;
                output_ << "✗ Error: " << err_resp->message << std::endl;
            } else {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize error response";
                output_ << "✗ Server error (could not parse error message)" << std::endl;
            }
            return std::nullopt;
        }

        BOOST_LOG_SEV(lg(), debug) << "Deserializing response";

        auto response = ResponseType::deserialize(response_result->payload());
        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
            output_ << "✗ Failed to parse response" << std::endl;
            return std::nullopt;
        }

        BOOST_LOG_SEV(lg(), info) << "Successfully processed request";
        return std::move(*response);
    }

    bool connect(std::string host, std::string port, std::string identifier);
    bool login(std::string username, std::string password);
    void disconnect();

private:
    std::ostream& output_;
    const std::optional<comms::net::client_options> connection_config_;
    const std::optional<config::login_options> login_config_;
    std::shared_ptr<comms::net::client> client_;
};

}

#endif
