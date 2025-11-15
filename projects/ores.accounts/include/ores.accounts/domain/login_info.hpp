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
#ifndef ORES_ACCOUNTS_DOMAIN_LOGIN_INFO_HPP
#define ORES_ACCOUNTS_DOMAIN_LOGIN_INFO_HPP

#include <chrono>
#include <boost/uuid/uuid.hpp>
#include <boost/asio/ip/address.hpp>

namespace ores::accounts::domain {

/**
 * @brief Represents login tracking and security information for an account.
 */
struct login_info final {
    /**
     * @brief Timestamp of the last successful login.
     */
    std::chrono::system_clock::time_point last_login;

    /**
     * @brief Foreign key referencing the associated account.
     */
    boost::uuids::uuid account_id;

    /**
     * @brief Count of consecutive failed login attempts since last successful login.
     */
    int failed_logins = 0;

    /**
     * @brief Flag indicating whether the account is locked due to security concerns.
     */
    bool locked = false;

    /**
     * @brief Flag indicating whether the user is currently logged in.
     */
    bool online = false;

    /**
     * @brief IP address from the last successful login.
     */
    boost::asio::ip::address last_ip;

    /**
     * @brief IP address from the most recent login attempt (successful or failed).
     */
    boost::asio::ip::address last_attempt_ip;
};

}

#endif
