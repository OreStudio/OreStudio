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
#ifndef ORES_IAM_REPOSITORY_ROLE_REPOSITORY_HPP
#define ORES_IAM_REPOSITORY_ROLE_REPOSITORY_HPP

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.iam/domain/role.hpp"

namespace ores::iam::repository {

/**
 * @brief Reads and writes roles to data storage.
 */
class role_repository {
private:
    inline static std::string_view logger_name =
        "ores.iam.repository.role_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit role_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes roles to database.
     */
    /**@{*/
    void write(const domain::role& role);
    void write(const std::vector<domain::role>& roles);
    /**@}*/

    /**
     * @brief Reads latest roles, possibly filtered by ID.
     */
    /**@{*/
    std::vector<domain::role> read_latest();
    std::vector<domain::role> read_latest(const boost::uuids::uuid& id);
    /**@}*/

    /**
     * @brief Reads latest role by name.
     */
    std::vector<domain::role> read_latest_by_name(const std::string& name);

    /**
     * @brief Reads multiple roles by their IDs in a single query.
     *
     * Uses IN clause to efficiently fetch multiple roles at once,
     * avoiding N+1 query issues.
     */
    std::vector<domain::role>
    read_latest_by_ids(const std::vector<boost::uuids::uuid>& ids);

    /**
     * @brief Deletes a role by closing its temporal validity.
     */
    void remove(const boost::uuids::uuid& role_id);

private:
    context ctx_;
};

}

#endif
