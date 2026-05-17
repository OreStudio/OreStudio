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
// testConnection() and signup() are isolated here so that their inline
// rfl::json::read<login_response> and rfl::json::read<signup_response>
// instantiations do not contribute to the per-TU template depth in
// ClientManager.cpp. MSVC C1202 fires when too many rfl types are
// reflected in a single translation unit.
#include "ores.qt/ClientManager.hpp"

#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_connect_error.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.iam.api/messaging/signup_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

LoginResult ClientManager::testConnection(
    const std::string& host,
    std::uint16_t port,
    const std::string& username,
    const std::string& password) {

    BOOST_LOG_SEV(lg(), info) << "Testing connection to " << host << ":" << port;

    try {
        ores::nats::service::nats_client temp_session;
        nats::config::nats_options opts;
        opts.url = "nats://" + host + ":" + std::to_string(port);
        opts.subject_prefix = subject_prefix_;
        temp_session.connect(std::move(opts));

        // Attempt login
        iam::messaging::login_request request{
            .principal = username,
            .password = password
        };
        const auto json_body = rfl::json::write(request);
        auto msg = temp_session.request(
            iam::messaging::login_request::nats_subject, json_body);
        temp_session.disconnect();

        const std::string_view data(
            reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
        auto resp = rfl::json::read<iam::messaging::login_response>(data);
        if (!resp || !resp->success) {
            const std::string err = resp ? resp->error_message : "Invalid response";
            return {.success = false, .error_message = QString::fromStdString(err)};
        }
        return {.success = true, .error_message = {}};

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Test connection failed: " << e.what();
        return {.success = false, .error_message = QString::fromStdString(e.what())};
    }
}

SignupResult ClientManager::signup(
    const std::string& host,
    std::uint16_t port,
    const std::string& username,
    const std::string& email,
    const std::string& password) {

    BOOST_LOG_SEV(lg(), info) << "Attempting signup to " << host << ":" << port;

    try {
        ores::nats::service::nats_client temp_session;
        nats::config::nats_options opts;
        opts.url = "nats://" + host + ":" + std::to_string(port);
        opts.subject_prefix = subject_prefix_;
        temp_session.connect(std::move(opts));

        iam::messaging::signup_request request{
            .principal = username,
            .password = password,
            .email = email
        };
        const auto json_body = rfl::json::write(request);
        auto msg = temp_session.request(
            iam::messaging::signup_request::nats_subject, json_body);
        temp_session.disconnect();

        const std::string_view data(
            reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
        auto resp = rfl::json::read<iam::messaging::signup_response>(data);
        if (!resp || !resp->success) {
            const std::string err = resp ? resp->message : "Invalid response";
            return {.success = false,
                    .error_message = QString::fromStdString(err)};
        }
        return {.success = true,
                .error_message = {},
                .username = QString::fromStdString(username)};

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Signup failed: " << e.what();
        return {.success = false,
                .error_message = QString::fromStdString(e.what())};
    }
}

}
