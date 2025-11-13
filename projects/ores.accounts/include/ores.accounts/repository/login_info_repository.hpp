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
#ifndef ORES_ACCOUNTS_REPOSITORY_LOGIN_INFO_REPOSITORY_HPP
#define ORES_ACCOUNTS_REPOSITORY_LOGIN_INFO_REPOSITORY_HPP

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/repository/context.hpp"
#include "ores.accounts/domain/login_info.hpp"

namespace ores::accounts::repository {

/**
 * @brief Reads and writes login tracking information off of data storage.
 */
class login_info_repository {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(
            "ores.accounts.repository.login_info_repository");
        return instance;
    }

    static void ensure_success(const auto result);

public:
    using context = ores::utility::repository::context;
    explicit login_info_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes login information to database (insert only).
     */
    void write(const std::vector<domain::login_info>& login_infos);

    /**
     * @brief Updates existing login information in database.
     */
    void update(const domain::login_info& login_info);

    /**
     * @brief Reads login information, possibly filtered by account ID.
     */
    /**@{*/
    std::vector<domain::login_info> read();
    std::vector<domain::login_info> read(const boost::uuids::uuid& account_id);
    /**@}*/

private:
    context ctx_;
};

}

#endif
