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
#ifndef ORES_ACCOUNTS_REPOSITORY_LOGINS_REPOSITORY_HPP
#define ORES_ACCOUNTS_REPOSITORY_LOGINS_REPOSITORY_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.utility/repository/context.hpp"
#include "ores.accounts/domain/logins.hpp"

namespace ores::accounts::repository {

/**
 * @brief Reads and writes login tracking information off of data storage.
 */
class logins_repository {
public:
    using context = ores::utility::repository::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes login information to database (insert only).
     */
    void write(context ctx, const std::vector<domain::logins>& logins);

    /**
     * @brief Updates existing login information in database.
     */
    void update(context ctx, const domain::logins& login_info);

    /**
     * @brief Reads login information, possibly filtered by account ID.
     */
    /**@{*/
    std::vector<domain::logins> read(context ctx);
    std::vector<domain::logins>
    read(context ctx, const boost::uuids::uuid& account_id);
    /**@}*/
};

}

#endif
