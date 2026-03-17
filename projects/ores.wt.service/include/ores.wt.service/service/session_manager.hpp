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
#ifndef ORES_WT_SERVICE_SESSION_MANAGER_HPP
#define ORES_WT_SERVICE_SESSION_MANAGER_HPP

#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include <boost/asio/ip/address.hpp>

namespace ores::wt::service {

/**
 * @brief Result of a login attempt.
 */
struct login_result {
    bool success = false;
    std::string error_message;
    boost::uuids::uuid account_id;
    std::string username;
    std::string email;
    bool password_reset_required = false;
};

/**
 * @brief Current session information.
 */
struct session_data {
    boost::uuids::uuid account_id;
    std::string username;
    std::string email;
};

/**
 * @brief Manages user authentication sessions for a single Wt application.
 *
 * Each WApplication instance should have its own session_manager. This class
 * wraps the authentication logic and maintains session state.
 */
class session_manager {
public:
    session_manager();

    login_result login(const std::string& username, const std::string& password,
                       const std::string& client_ip);

    void logout();

    bool is_logged_in() const { return session_.has_value(); }

    const std::optional<session_data>& session() const { return session_; }

    bool has_permission(const std::string& permission) const;

    login_result create_bootstrap_admin(const std::string& username,
                                        const std::string& email,
                                        const std::string& password);

private:
    std::optional<session_data> session_;
};

}

#endif
