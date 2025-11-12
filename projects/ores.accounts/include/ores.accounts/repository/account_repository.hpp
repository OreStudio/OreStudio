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
#ifndef ORES_ACCOUNTS_REPOSITORY_ACCOUNT_REPOSITORY_HPP
#define ORES_ACCOUNTS_REPOSITORY_ACCOUNT_REPOSITORY_HPP

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/repository/context.hpp"
#include "ores.accounts/domain/account.hpp"

namespace ores::accounts::repository {

/**
 * @brief Reads and writes accounts off of data storage.
 */
class account_repository {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(
            "ores.accounts.repository.account_repository");
        return instance;
    }

    void ensure_success(const auto result);
    auto make_timestamp(const std::string& s);
    const std::string max_timestamp = "9999-12-31 23:59:59";

public:
    using context = ores::utility::repository::context;

    explicit account_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes accounts to database. Expects the account set to have
     * unique IDs.
     */
    /**@{*/
    void write(const domain::account& account);
    void write(const std::vector<domain::account>& accounts);
    /**@}*/

    /**
     * @brief Reads latest accounts, possibly filtered by ID.
     */
    /**@{*/
    std::vector<domain::account> read_latest();
    std::vector<domain::account> read_latest(const boost::uuids::uuid& id);
    /**@}*/

    /**
     * @brief Reads all accounts, possibly filtered by ID.
     */
    /**@{*/
    std::vector<domain::account> read_all();
    std::vector<domain::account> read_all(const boost::uuids::uuid& id);
    /**@}*/

    /**
     * @brief Reads the latest account by username.
     */
    std::vector<domain::account>
    read_latest_by_username(const std::string& username);

private:
    context ctx_;
};

}

#endif
