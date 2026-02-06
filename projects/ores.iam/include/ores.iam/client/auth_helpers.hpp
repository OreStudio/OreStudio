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
#ifndef ORES_IAM_CLIENT_AUTH_HELPERS_HPP
#define ORES_IAM_CLIENT_AUTH_HELPERS_HPP

#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/net/client_session.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::iam::client {

/**
 * @brief Result of a login operation.
 *
 * Contains all information returned from a successful login, or the error
 * message on failure.
 */
struct login_result {
    bool success = false;
    std::string error_message;
    boost::uuids::uuid account_id;
    std::string username;
    std::string email;
    std::string tenant_name;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    bool password_reset_required = false;
};

/**
 * @brief Perform login and update session state.
 *
 * Sends login_request, processes response, and calls
 * session.set_session_info() on success.
 *
 * @param session The client session to use for the request and state update.
 * @param principal The user principal (username or username@hostname).
 * @param password The user's password.
 * @return login_result containing the result of the operation.
 */
login_result login(comms::net::client_session& session,
    const std::string& principal,
    const std::string& password);

/**
 * @brief Result of a logout operation.
 *
 * Contains success status and error message on failure.
 */
struct logout_result {
    bool success = false;
    std::string error_message;
};

/**
 * @brief Perform logout with proper cleanup.
 *
 * Sends logout_request, clears session info, and disconnects
 * to prevent auto-reconnect after server closes connection.
 *
 * @param session The client session to use for the request.
 * @return logout_result containing the result of the operation.
 */
logout_result logout(comms::net::client_session& session);

}

#endif
