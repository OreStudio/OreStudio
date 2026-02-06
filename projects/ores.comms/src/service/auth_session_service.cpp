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
#include "ores.comms/service/auth_session_service.hpp"

#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/uuid/uuid_v7_generator.hpp"

namespace ores::comms::service {

using namespace ores::logging;

std::optional<session_info>
auth_session_service::get_session(const std::string& remote_address) const {
    std::lock_guard lock(session_mutex_);
    auto it = sessions_.find(remote_address);
    if (it != sessions_.end() && it->second) {
        return session_info{
            .account_id = it->second->account_id,
            .tenant_id = it->second->tenant_id,
            .username = it->second->username
        };
    }
    return std::nullopt;
}

std::shared_ptr<session_data>
auth_session_service::get_session_data(const std::string& remote_address) const {
    std::lock_guard lock(session_mutex_);
    auto it = sessions_.find(remote_address);
    if (it != sessions_.end()) {
        return it->second;
    }
    return nullptr;
}

bool auth_session_service::is_authenticated(const std::string& remote_address) const {
    std::lock_guard lock(session_mutex_);
    return sessions_.contains(remote_address);
}

void auth_session_service::store_session(const std::string& remote_address,
    session_info info) {
    // Create a minimal session object from the legacy session_info
    auto session = std::make_shared<session_data>();
    session->id = utility::uuid::uuid_v7_generator{}();
    session->account_id = info.account_id;
    session->tenant_id = info.tenant_id;
    session->username = std::move(info.username);
    session->start_time = std::chrono::system_clock::now();

    store_session_data(remote_address, std::move(session));
}

void auth_session_service::store_session_data(const std::string& remote_address,
    std::shared_ptr<session_data> session) {
    std::lock_guard lock(session_mutex_);
    BOOST_LOG_SEV(lg(), info) << "Storing session for " << remote_address
                              << " session_id=" << session->id
                              << " account_id=" << session->account_id;
    sessions_[remote_address] = std::move(session);
}

void auth_session_service::update_session_bytes(const std::string& remote_address,
    std::uint64_t bytes_sent, std::uint64_t bytes_received) {
    std::lock_guard lock(session_mutex_);
    auto it = sessions_.find(remote_address);
    if (it != sessions_.end() && it->second) {
        it->second->bytes_sent = bytes_sent;
        it->second->bytes_received = bytes_received;
    }
}

std::shared_ptr<session_data>
auth_session_service::remove_session(const std::string& remote_address) {
    std::lock_guard lock(session_mutex_);
    auto it = sessions_.find(remote_address);
    if (it != sessions_.end()) {
        auto session = std::move(it->second);
        BOOST_LOG_SEV(lg(), info) << "Removing session for " << remote_address
                                  << " session_id=" << session->id
                                  << " account_id=" << session->account_id;
        sessions_.erase(it);
        return session;
    }
    return nullptr;
}

std::vector<std::shared_ptr<session_data>>
auth_session_service::clear_all_sessions() {
    std::lock_guard lock(session_mutex_);
    BOOST_LOG_SEV(lg(), info) << "Clearing all sessions (count="
                              << sessions_.size() << ")";

    std::vector<std::shared_ptr<session_data>> result;
    result.reserve(sessions_.size());
    for (const auto& [_, session] : sessions_) {
        result.push_back(session);
    }
    sessions_.clear();
    return result;
}

std::vector<std::shared_ptr<session_data>>
auth_session_service::get_all_sessions() const {
    std::lock_guard lock(session_mutex_);
    std::vector<std::shared_ptr<session_data>> result;
    result.reserve(sessions_.size());
    for (const auto& [_, session] : sessions_) {
        result.push_back(session);
    }
    return result;
}

void auth_session_service::store_client_info(const std::string& remote_address,
    client_info info) {
    std::lock_guard lock(client_info_mutex_);
    BOOST_LOG_SEV(lg(), debug) << "Storing client info for " << remote_address
                               << " client=" << info.client_identifier
                               << " version=" << info.client_version_major
                               << "." << info.client_version_minor;
    client_infos_[remote_address] = std::move(info);
}

std::optional<client_info>
auth_session_service::get_client_info(const std::string& remote_address) const {
    std::lock_guard lock(client_info_mutex_);
    auto it = client_infos_.find(remote_address);
    if (it != client_infos_.end()) {
        return it->second;
    }
    return std::nullopt;
}

void auth_session_service::remove_client_info(const std::string& remote_address) {
    std::lock_guard lock(client_info_mutex_);
    auto it = client_infos_.find(remote_address);
    if (it != client_infos_.end()) {
        BOOST_LOG_SEV(lg(), debug) << "Removing client info for " << remote_address;
        client_infos_.erase(it);
    }
}

bool auth_session_service::requires_authentication(messaging::message_type type) {
    using messaging::message_type;

    // Messages that do NOT require authentication
    switch (type) {
    // Ping/Pong - always allowed for connection health
    case message_type::ping:
    case message_type::pong:
    // Handshake - required before login
    case message_type::handshake_request:
    case message_type::handshake_ack:
    // Login - obviously can't require auth
    case message_type::login_request:
    // Signup - self-registration (flag-protected at handler level)
    case message_type::signup_request:
    // Bootstrap operations - only allowed in bootstrap mode anyway
    case message_type::create_initial_admin_request:
    case message_type::bootstrap_status_request:
        return false;
    default:
        // All other messages require authentication
        return true;
    }
}

std::expected<void, ores::utility::serialization::error_code>
auth_session_service::authorize_request(messaging::message_type type,
    const std::string& remote_address) const {

    // Check if authentication is required
    if (!requires_authentication(type)) {
        return {};  // No auth needed, allow
    }

    // Check if user is authenticated
    auto session = get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn)
            << "Authentication failed for " << type
            << " from " << remote_address << ": not authenticated";
        return std::unexpected(ores::utility::serialization::error_code::authentication_failed);
    }

    // Permission-based authorization is handled at the handler level
    // using authorization_service.has_permission()

    BOOST_LOG_SEV(lg(), debug)
        << "Authentication granted for " << type
        << " from " << remote_address;
    return {};
}

}
