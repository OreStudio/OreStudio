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
#ifndef ORES_IAM_REPOSITORY_PERMISSION_REPOSITORY_HPP
#define ORES_IAM_REPOSITORY_PERMISSION_REPOSITORY_HPP

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.iam/domain/permission.hpp"

namespace ores::iam::repository {

/**
 * @brief Reads and writes permissions to data storage.
 */
class permission_repository {
private:
    inline static std::string_view logger_name =
        "ores.iam.repository.permission_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit permission_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes permissions to database.
     */
    /**@{*/
    void write(const domain::permission& permission);
    void write(const std::vector<domain::permission>& permissions);
    /**@}*/

    /**
     * @brief Reads latest permissions, possibly filtered by ID.
     */
    /**@{*/
    std::vector<domain::permission> read_latest();
    std::vector<domain::permission> read_latest(const boost::uuids::uuid& id);
    /**@}*/

    /**
     * @brief Reads latest permissions with pagination support.
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     * @return Vector of permissions within the specified range
     */
    std::vector<domain::permission> read_latest(std::uint32_t offset,
                                                std::uint32_t limit);

    /**
     * @brief Gets the total count of active permissions.
     * @return Total number of permissions with valid_to == max_timestamp
     */
    std::uint32_t get_total_permission_count();

    /**
     * @brief Reads latest permission by code.
     */
    std::vector<domain::permission> read_latest_by_code(const std::string& code);

    /**
     * @brief Deletes a permission by closing its temporal validity.
     */
    void remove(const boost::uuids::uuid& permission_id);

private:
    context ctx_;
};

}

#endif
