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
#ifndef ORES_ACCOUNTS_DOMAIN_ACCOUNT_HPP
#define ORES_ACCOUNTS_DOMAIN_ACCOUNT_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>
#include <iosfwd>
#include <boost/uuid/uuid.hpp>

namespace ores::accounts::domain {

/**
 * @brief Represents a user account in the system.
 */
struct account final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version;

    /**
     * @brief Username of the user who last modified this account.
     */
    std::string modified_by;

    /**
     * @brief Unique identifier for the account.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique username for login purposes.
     */
    std::string username;

    /**
     * @brief Hashed password for secure authentication.
     */
    std::string password_hash;

    /**
     * @brief Salt used in password hashing for additional security.
     */
    std::string password_salt;

    /**
     * @brief Time-based One-Time Password secret for two-factor authentication.
     */
    std::string totp_secret;

    /**
     * @brief Email address associated with the account.
     */
    std::string email;

    /**
     * @brief Flag indicating whether the account has administrative privileges.
     */
    bool is_admin;
};

std::ostream& operator<<(std::ostream& s, const account& v);

}

#endif
