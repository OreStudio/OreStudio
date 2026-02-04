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
#include "ores.iam/client/auth_helpers.hpp"

#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.iam/messaging/login_protocol.hpp"

namespace ores::iam::client {

namespace {

inline std::string_view logger_name = "ores.iam.client.auth_helpers";

auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger(logger_name);
    return instance;
}

}

login_result login(comms::net::client_session& session,
    const std::string& principal,
    const std::string& password) {
    using namespace ores::logging;

    BOOST_LOG_SEV(lg(), debug) << "Initiating login request for principal: "
                               << principal;

    login_result result;

    using iam::messaging::login_request;
    auto response_result = session.process_request(login_request{
        .principal = principal,
        .password = password
    });

    if (!response_result) {
        BOOST_LOG_SEV(lg(), error) << "Login request failed: "
                                   << comms::net::to_string(response_result.error());
        result.error_message = comms::net::to_string(response_result.error());
        return result;
    }

    const auto& response = *response_result;
    if (!response.success) {
        BOOST_LOG_SEV(lg(), warn) << "Login failed: " << response.error_message;
        result.error_message = response.error_message;
        return result;
    }

    BOOST_LOG_SEV(lg(), info) << "Login successful for user: " << response.username
                              << ", tenant: " << response.tenant_name
                              << " (" << response.tenant_id << ")";

    // Populate result
    result.success = true;
    result.account_id = response.account_id;
    result.username = response.username;
    result.email = response.email;
    result.tenant_name = response.tenant_name;
    result.tenant_id = response.tenant_id;
    result.password_reset_required = response.password_reset_required;

    // Update session state
    comms::net::client_session_info info{
        .account_id = response.account_id,
        .username = response.username
    };
    session.set_session_info(std::move(info));

    return result;
}

bool logout(comms::net::client_session& session) {
    using namespace ores::logging;

    BOOST_LOG_SEV(lg(), debug) << "Processing logout request.";

    if (!session.is_logged_in()) {
        BOOST_LOG_SEV(lg(), warn) << "Logout called but not logged in.";
        return false;
    }

    using iam::messaging::logout_request;
    using iam::messaging::logout_response;
    using comms::messaging::message_type;
    auto response_result = session.process_authenticated_request<
        logout_request, logout_response, message_type::logout_request>(
            logout_request{});

    if (!response_result) {
        BOOST_LOG_SEV(lg(), error) << "Logout request failed: "
                                   << comms::net::to_string(response_result.error());
        return false;
    }

    const auto& response = *response_result;
    if (!response.success) {
        BOOST_LOG_SEV(lg(), warn) << "Logout failed: " << response.message;
        return false;
    }

    BOOST_LOG_SEV(lg(), info) << "Logout successful.";
    session.clear_session_info();
    // Disconnect to prevent auto-reconnect after server closes connection
    session.disconnect();
    return true;
}

}
