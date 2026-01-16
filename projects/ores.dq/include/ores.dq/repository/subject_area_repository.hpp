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
#ifndef ORES_DQ_REPOSITORY_SUBJECT_AREA_REPOSITORY_HPP
#define ORES_DQ_REPOSITORY_SUBJECT_AREA_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/subject_area.hpp"

namespace ores::dq::repository {

/**
 * @brief Reads and writes subject_areas to data storage.
 */
class subject_area_repository {
private:
    inline static std::string_view logger_name =
        "ores.dq.repository.subject_area_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit subject_area_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes subject_areas to database.
     */
    /**@{*/
    void write(const domain::subject_area& subject_area);
    void write(const std::vector<domain::subject_area>& subject_areas);
    /**@}*/

    /**
     * @brief Reads latest subject_areas.
     */
    std::vector<domain::subject_area> read_latest();

    /**
     * @brief Reads latest subject_area by composite key.
     */
    std::vector<domain::subject_area>
    read_latest(const std::string& name, const std::string& domain_name);

    /**
     * @brief Reads latest subject_areas by domain name.
     */
    std::vector<domain::subject_area>
    read_latest_by_domain(const std::string& domain_name);

    /**
     * @brief Reads latest subject_areas with pagination support.
     */
    std::vector<domain::subject_area>
    read_latest(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active subject_areas.
     */
    std::uint32_t get_total_count();

    /**
     * @brief Reads all historical versions of a subject_area by composite key.
     */
    std::vector<domain::subject_area>
    read_all(const std::string& name, const std::string& domain_name);

    /**
     * @brief Deletes a subject_area by closing its temporal validity.
     */
    void remove(const std::string& name, const std::string& domain_name);

private:
    context ctx_;
};

}

#endif
