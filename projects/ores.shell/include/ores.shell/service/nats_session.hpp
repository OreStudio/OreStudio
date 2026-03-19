/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_SHELL_SERVICE_NATS_SESSION_HPP
#define ORES_SHELL_SERVICE_NATS_SESSION_HPP

#include <chrono>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/domain/message.hpp"

namespace ores::nats::service { class client; }

namespace ores::shell::service {

/**
 * @brief Wraps a NATS client with login state for the shell REPL.
 */
class nats_session {
private:
    inline static std::string_view logger_name =
        "ores.shell.service.nats_session";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    struct login_info {
        std::string jwt;
        std::string username;
        std::string tenant_id;
        std::string tenant_name;
    };

    /**
     * @brief Connect to NATS using the given options. Throws on failure.
     *
     * The options include the server URL and the optional subject prefix
     * (see @c nats_options::subject_prefix).
     */
    void connect(nats::config::nats_options opts);

    /**
     * @brief Disconnect from NATS.
     */
    void disconnect();

    [[nodiscard]] bool is_connected() const noexcept;

    /**
     * @brief Store authentication info after login.
     */
    void set_auth(login_info info);

    /**
     * @brief Clear authentication info after logout.
     */
    void clear_auth();

    [[nodiscard]] bool is_logged_in() const noexcept;

    /**
     * @brief Return current auth info. Precondition: is_logged_in().
     */
    [[nodiscard]] const login_info& auth() const;

    /**
     * @brief Synchronous NATS request (unauthenticated).
     *
     * @param subject NATS subject to send to
     * @param json_body JSON request body
     * @return Reply message
     * @throws std::runtime_error on timeout or transport error
     */
    [[nodiscard]] ores::nats::message request(std::string_view subject,
                                              std::string_view json_body);

    /**
     * @brief Synchronous NATS request with JWT authorization header.
     *
     * @param subject NATS subject to send to
     * @param json_body JSON request body
     * @param timeout  Request timeout (default: 30 seconds)
     * @return Reply message
     * @throws std::runtime_error on timeout or transport error
     */
    [[nodiscard]] ores::nats::message authenticated_request(
        std::string_view subject,
        std::string_view json_body,
        std::chrono::milliseconds timeout = std::chrono::seconds(30));

    /**
     * @brief Get the underlying NATS client. May be null if not connected.
     */
    [[nodiscard]] std::shared_ptr<ores::nats::service::client> get_client() const;

private:
    std::shared_ptr<ores::nats::service::client> client_;
    std::optional<login_info> auth_;
};

}

#endif
