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
#ifndef ORES_DQ_REPOSITORY_DATA_DOMAIN_REPOSITORY_HPP
#define ORES_DQ_REPOSITORY_DATA_DOMAIN_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/data_domain.hpp"

namespace ores::dq::repository {

/**
 * @brief Reads and writes data_domains to data storage.
 */
class data_domain_repository {
private:
    inline static std::string_view logger_name =
        "ores.dq.repository.data_domain_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit data_domain_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes data_domains to database.
     */
    /**@{*/
    void write(const domain::data_domain& data_domain);
    void write(const std::vector<domain::data_domain>& data_domains);
    /**@}*/

    /**
     * @brief Reads latest data_domains, possibly filtered by name.
     */
    /**@{*/
    std::vector<domain::data_domain> read_latest();
    std::vector<domain::data_domain> read_latest(const std::string& name);
    /**@}*/

    /**
     * @brief Reads latest data_domains with pagination support.
     */
    std::vector<domain::data_domain>
    read_latest(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active data_domains.
     */
    std::uint32_t get_total_count();

    /**
     * @brief Reads all historical versions of a data_domain by name.
     */
    std::vector<domain::data_domain> read_all(const std::string& name);

    /**
     * @brief Deletes a data_domain by closing its temporal validity.
     */
    void remove(const std::string& name);

private:
    context ctx_;
};

}

#endif
