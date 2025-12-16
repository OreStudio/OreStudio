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
#ifndef ORES_ACCOUNTS_DOMAIN_ACCOUNT_VERSION_HPP
#define ORES_ACCOUNTS_DOMAIN_ACCOUNT_VERSION_HPP

#include <string>
#include "ores.accounts/domain/account.hpp"

namespace ores::accounts::domain {

/**
 * @brief Represents a specific version of an account with metadata.
 */
struct account_version final {
    /**
     * @brief The account data at this version.
     */
    account data;

    /**
     * @brief Version number (1-based, higher is newer).
     */
    int version_number;

    /**
     * @brief Username of the person who recorded this version in the system.
     */
    std::string recorded_by;

    /**
     * @brief Timestamp when this version was recorded in the system.
     */
    std::string recorded_at;

    /**
     * @brief Summary of changes made in this version.
     *
     * Examples: "Created account", "Modified 2 fields", "Updated email"
     */
    std::string change_summary;
};

}

#endif
