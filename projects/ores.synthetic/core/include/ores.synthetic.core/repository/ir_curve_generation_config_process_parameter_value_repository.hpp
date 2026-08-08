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
#ifndef ORES_SYNTHETIC_CORE_REPOSITORY_IR_CURVE_GENERATION_CONFIG_PROCESS_PARAMETER_VALUE_REPOSITORY_HPP
#define ORES_SYNTHETIC_CORE_REPOSITORY_IR_CURVE_GENERATION_CONFIG_PROCESS_PARAMETER_VALUE_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value.hpp"
#include "ores.synthetic.core/export.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::synthetic::repository {

/**
 * @brief Reads and writes IR curve generation config process parameter values to data storage.
 */
class ORES_SYNTHETIC_CORE_EXPORT ir_curve_generation_config_process_parameter_value_repository {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.repository.ir_curve_generation_config_process_parameter_value_repository";

    [[nodiscard]] static auto& lg() {
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
     * @brief Writes IR curve generation config process parameter values to database.
     */
    /**@{*/
    void write(context ctx, const domain::ir_curve_generation_config_process_parameter_value& v);
    void write(context ctx,
               const std::vector<domain::ir_curve_generation_config_process_parameter_value>& v);
    /**@}*/

    /**
     * @brief Reads latest IR curve generation config process parameter values, possibly filtered by
     * primary key.
     */
    /**@{*/
    std::vector<domain::ir_curve_generation_config_process_parameter_value>
    read_latest(context ctx);
    std::vector<domain::ir_curve_generation_config_process_parameter_value>
    read_latest(context ctx, const std::string& id);
    /**@}*/

    /**
     * @brief Reads all IR curve generation config process parameter values, possibly filtered by
     * primary key.
     */
    std::vector<domain::ir_curve_generation_config_process_parameter_value>
    read_all(context ctx, const std::string& id);

    /**
     * @brief Reads a single IR curve generation config process parameter value as it stood at a
     * specific version — the version's own [valid_from, valid_to) window is returned verbatim, so
     * the caller can compose child entities "as of" the same window. See the "Temporal composite
     * entity versioning" architecture doc.
     * @param ctx Repository context with database connection
     * @param version The version to fetch
     */
    std::optional<domain::ir_curve_generation_config_process_parameter_value>
    read_at_version(context ctx, const std::string& id, std::uint32_t version);


    /**
     * @brief Reads latest IR curve generation config process parameter values with pagination
     * support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::ir_curve_generation_config_process_parameter_value>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active IR curve generation config process parameter values.
     * @param ctx Repository context with database connection
     * @return Total number of active IR curve generation config process parameter values
     */
    std::uint32_t get_total_process_parameter_value_count(context ctx);

    /**
     * @brief Deletes a IR curve generation config process parameter value by closing its temporal
     * validity.
     */
    void remove(context ctx, const std::string& id);

    /**
     * @brief Deletes IR curve generation config process parameter values by closing their temporal
     * validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);
};

}

#endif
