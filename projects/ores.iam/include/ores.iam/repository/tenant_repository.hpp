/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_IAM_REPOSITORY_TENANT_REPOSITORY_HPP
#define ORES_IAM_REPOSITORY_TENANT_REPOSITORY_HPP

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.iam/domain/tenant.hpp"

namespace ores::iam::repository {

/**
 * @brief Reads and writes tenants to data storage.
 */
class tenant_repository {
private:
    inline static std::string_view logger_name =
        "ores.iam.repository.tenant_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit tenant_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes a tenant to database.
     */
    /**@{*/
    void write(const domain::tenant& tenant);
    void write(const std::vector<domain::tenant>& tenants);
    /**@}*/

    /**
     * @brief Reads all latest tenants.
     */
    std::vector<domain::tenant> read_latest();

    /**
     * @brief Reads a specific tenant by ID.
     */
    std::vector<domain::tenant> read_latest(const boost::uuids::uuid& id);

    /**
     * @brief Reads a specific tenant by code.
     */
    std::vector<domain::tenant> read_latest_by_code(const std::string& code);

    /**
     * @brief Reads a specific tenant by hostname.
     */
    std::vector<domain::tenant> read_latest_by_hostname(const std::string& hostname);

    /**
     * @brief Reads all historical versions of a tenant.
     */
    std::vector<domain::tenant> read_history(const boost::uuids::uuid& id);

    /**
     * @brief Deletes a tenant by closing its temporal validity.
     */
    void remove(const boost::uuids::uuid& tenant_id);

private:
    context ctx_;
};

}

#endif
