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
#include "ores.iam/service/service_session_service.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/asio/ip/address.hpp>

namespace ores::iam::service {

using namespace ores::logging;

service_session_service::service_session_service(context ctx)
    : session_repo_(ctx),
      account_repo_(ctx) {

    BOOST_LOG_SEV(lg(), debug) << "Service session service initialized";
}

std::optional<domain::account>
service_session_service::get_service_account(const std::string& username) {
    BOOST_LOG_SEV(lg(), debug) << "Looking up service account: " << username;

    auto accounts = account_repo_.read_latest_by_username(username);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Service account not found: " << username;
        return std::nullopt;
    }

    const auto& account = accounts[0];

    // Verify this is a service account (not a user)
    if (account.account_type == "user") {
        BOOST_LOG_SEV(lg(), warn) << "Account '" << username
                                  << "' is a user account, not a service account";
        return std::nullopt;
    }

    return account;
}

std::optional<domain::session>
service_session_service::start_service_session(
    const std::string& username,
    const std::string& client_identifier,
    domain::session_protocol protocol) {

    BOOST_LOG_SEV(lg(), debug) << "Starting service session for username: "
                               << username << " with client: " << client_identifier;

    auto account_opt = get_service_account(username);
    if (!account_opt) {
        return std::nullopt;
    }

    return start_service_session(account_opt->id, client_identifier, protocol);
}

std::optional<domain::session>
service_session_service::start_service_session(
    const boost::uuids::uuid& account_id,
    const std::string& client_identifier,
    domain::session_protocol protocol) {

    BOOST_LOG_SEV(lg(), debug) << "Starting service session for account ID: "
                               << boost::uuids::to_string(account_id);

    // Verify account exists and is a service account
    auto accounts = account_repo_.read_latest(account_id);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Account not found: "
                                  << boost::uuids::to_string(account_id);
        return std::nullopt;
    }

    const auto& account = accounts[0];

    if (account.account_type == "user") {
        BOOST_LOG_SEV(lg(), warn) << "Cannot create service session for user account: "
                                  << account.username;
        return std::nullopt;
    }

    // Generate session ID and create session
    auto session_id = uuid_generator_();
    auto now = std::chrono::system_clock::now();

    domain::session session {
        .tenant_id = account.tenant_id,
        .id = session_id,
        .account_id = account_id,
        .start_time = now,
        .end_time = std::nullopt,
        .client_ip = boost::asio::ip::make_address("127.0.0.1"),
        .client_identifier = client_identifier,
        .client_version_major = 1,
        .client_version_minor = 0,
        .bytes_sent = 0,
        .bytes_received = 0,
        .country_code = "",
        .protocol = protocol,
        .username = account.username
    };

    session_repo_.create(session);

    BOOST_LOG_SEV(lg(), info) << "Started service session: "
                              << boost::uuids::to_string(session_id)
                              << " for account: " << account.username
                              << " (type: " << account.account_type << ")";

    return session;
}

void service_session_service::end_service_session(
    const boost::uuids::uuid& session_id,
    const std::chrono::system_clock::time_point& start_time,
    std::uint64_t bytes_sent,
    std::uint64_t bytes_received) {

    BOOST_LOG_SEV(lg(), debug) << "Ending service session: "
                               << boost::uuids::to_string(session_id);

    auto now = std::chrono::system_clock::now();
    session_repo_.end_session(session_id, start_time, now,
                              bytes_sent, bytes_received);

    BOOST_LOG_SEV(lg(), info) << "Ended service session: "
                              << boost::uuids::to_string(session_id);
}

}
