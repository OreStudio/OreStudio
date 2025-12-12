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
#include "ores.comms/net/client_session.hpp"

namespace ores::comms::net {

using namespace ores::utility::log;

client_session::~client_session() {
    if (is_connected()) {
        disconnect();
    }
}

std::expected<void, client_session_error>
client_session::connect(client_options options) {
    BOOST_LOG_SEV(lg(), info) << "Connecting to " << options.host << ":"
                              << options.port << " (identifier: "
                              << options.client_identifier << ")";

    if (client_ && client_->is_connected()) {
        BOOST_LOG_SEV(lg(), info) << "Disconnecting existing connection";
        client_->disconnect();
        session_info_.reset();
    }

    try {
        client_ = std::make_shared<client>(std::move(options));
        client_->connect_sync();
        BOOST_LOG_SEV(lg(), info) << "Successfully connected.";
        return {};
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Connection failed: " << e.what();
        client_.reset();
        return std::unexpected(client_session_error::not_connected);
    }
}

void client_session::disconnect() {
    if (!client_) {
        BOOST_LOG_SEV(lg(), warn) << "No client instance.";
        return;
    }

    if (!client_->is_connected()) {
        BOOST_LOG_SEV(lg(), debug) << "Already disconnected.";
        session_info_.reset();
        return;
    }

    // Clear session info on disconnect
    session_info_.reset();

    client_->disconnect();
    BOOST_LOG_SEV(lg(), info) << "Disconnected from server.";
}

bool client_session::is_connected() const noexcept {
    return client_ && client_->is_connected();
}

std::string to_string(client_session_error error) {
    switch (error) {
    case client_session_error::not_connected:
        return "Not connected to server";
    case client_session_error::not_logged_in:
        return "Not logged in";
    case client_session_error::login_required:
        return "Login required for this operation";
    case client_session_error::admin_required:
        return "Admin privileges required for this operation";
    case client_session_error::request_failed:
        return "Request failed";
    case client_session_error::deserialization_failed:
        return "Failed to parse server response";
    case client_session_error::server_error:
        return "Server returned an error";
    default:
        return "Unknown error";
    }
}

}
