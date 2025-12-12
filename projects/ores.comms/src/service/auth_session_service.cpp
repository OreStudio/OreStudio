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

namespace ores::comms::service {

using namespace ores::utility::log;

std::optional<session_info>
auth_session_service::get_session(const std::string& remote_address) const {
    std::lock_guard lock(session_mutex_);
    auto it = sessions_.find(remote_address);
    if (it != sessions_.end()) {
        return it->second;
    }
    return std::nullopt;
}

bool auth_session_service::is_authenticated(const std::string& remote_address) const {
    std::lock_guard lock(session_mutex_);
    return sessions_.contains(remote_address);
}

bool auth_session_service::is_admin(const std::string& remote_address) const {
    std::lock_guard lock(session_mutex_);
    auto it = sessions_.find(remote_address);
    if (it != sessions_.end()) {
        return it->second.is_admin;
    }
    return false;
}

void auth_session_service::store_session(const std::string& remote_address,
    session_info session) {
    std::lock_guard lock(session_mutex_);
    BOOST_LOG_SEV(lg(), info) << "Storing session for " << remote_address
                              << " account_id=" << session.account_id
                              << " is_admin=" << session.is_admin;
    sessions_[remote_address] = std::move(session);
}

void auth_session_service::remove_session(const std::string& remote_address) {
    std::lock_guard lock(session_mutex_);
    auto it = sessions_.find(remote_address);
    if (it != sessions_.end()) {
        BOOST_LOG_SEV(lg(), info) << "Removing session for " << remote_address
                                  << " account_id=" << it->second.account_id;
        sessions_.erase(it);
    }
}

void auth_session_service::clear_all_sessions() {
    std::lock_guard lock(session_mutex_);
    BOOST_LOG_SEV(lg(), info) << "Clearing all sessions (count="
                              << sessions_.size() << ")";
    sessions_.clear();
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
    // Bootstrap operations - only allowed in bootstrap mode anyway
    case message_type::create_initial_admin_request:
    case message_type::bootstrap_status_request:
        return false;
    default:
        // All other messages require authentication
        return true;
    }
}

bool auth_session_service::requires_admin(messaging::message_type type) {
    using messaging::message_type;

    // Messages that require admin privileges
    switch (type) {
    // Account management (creating, deleting, unlocking accounts)
    case message_type::create_account_request:
    case message_type::delete_account_request:
    case message_type::unlock_account_request:
    // Currency modifications
    case message_type::save_currency_request:
    case message_type::delete_currency_request:
        return true;
    default:
        return false;
    }
}

std::expected<void, messaging::error_code>
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
            << "Authorization failed for " << type
            << " from " << remote_address << ": not authenticated";
        return std::unexpected(messaging::error_code::authentication_failed);
    }

    // Check if admin is required
    if (requires_admin(type) && !session->is_admin) {
        BOOST_LOG_SEV(lg(), warn)
            << "Authorization failed for " << type
            << " from " << remote_address << ": admin required";
        return std::unexpected(messaging::error_code::authorization_failed);
    }

    BOOST_LOG_SEV(lg(), debug)
        << "Authorization granted for " << type
        << " from " << remote_address;
    return {};
}

}
