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
#ifndef ORES_SYNTHETIC_CORE_REPOSITORY_GMM_COMPONENT_REPOSITORY_HPP
#define ORES_SYNTHETIC_CORE_REPOSITORY_GMM_COMPONENT_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.core/export.hpp"
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::synthetic::repository {

/**
 * @brief Reads and writes GMM components off of data storage.
 */
class ORES_SYNTHETIC_CORE_EXPORT gmm_component_repository {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.repository.gmm_component_repository";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes components to database. Expects unique ids.
     */
    /**@{*/
    void write(context ctx, const domain::gmm_component& component);
    void write(context ctx, const std::vector<domain::gmm_component>& components);
    /**@}*/

    /**
     * @brief Reads latest components, possibly filtered by id.
     */
    /**@{*/
    std::vector<domain::gmm_component> read_latest(context ctx);
    std::vector<domain::gmm_component> read_latest(context ctx, const std::string& id);
    /**@}*/

    /**
     * @brief Reads latest components with pagination support.
     */
    std::vector<domain::gmm_component>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active components.
     */
    std::uint32_t get_total_component_count(context ctx);

    /**
     * @brief Reads components at the supplied time point, possibly filtered by id.
     */
    /**@{*/
    std::vector<domain::gmm_component>
    read_at_timepoint(context ctx, const std::string& as_of);
    std::vector<domain::gmm_component>
    read_at_timepoint(context ctx, const std::string& as_of, const std::string& id);
    /**@}*/

    /**
     * @brief Reads all components, possibly filtered by id.
     */
    /**@{*/
    std::vector<domain::gmm_component> read_all(context ctx);
    std::vector<domain::gmm_component> read_all(context ctx, const std::string& id);
    /**@}*/

    /**
     * @brief Deletes a component by closing its temporal validity.
     */
    void remove(context ctx, const std::string& id);

    /**
     * @brief Deletes components by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);
};

}

#endif
