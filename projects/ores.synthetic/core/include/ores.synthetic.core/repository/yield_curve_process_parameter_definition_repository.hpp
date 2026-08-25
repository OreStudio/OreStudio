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
#ifndef ORES_SYNTHETIC_CORE_REPOSITORY_YIELD_CURVE_PROCESS_PARAMETER_DEFINITION_REPOSITORY_HPP
#define ORES_SYNTHETIC_CORE_REPOSITORY_YIELD_CURVE_PROCESS_PARAMETER_DEFINITION_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition.hpp"
#include "ores.synthetic.core/export.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::synthetic::repository {

/**
 * @brief Reads and writes yield curve process parameter definitions to data storage.
 */
class ORES_SYNTHETIC_CORE_EXPORT yield_curve_process_parameter_definition_repository {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.repository.yield_curve_process_parameter_definition_repository";

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
     * @brief Writes yield curve process parameter definitions to database.
     */
    /**@{*/
    void write(context ctx, const domain::yield_curve_process_parameter_definition& v);
    void write(context ctx, const std::vector<domain::yield_curve_process_parameter_definition>& v);
    /**@}*/

    /**
     * @brief Reads latest yield curve process parameter definitions, possibly filtered by primary
     * key.
     */
    /**@{*/
    std::vector<domain::yield_curve_process_parameter_definition> read_latest(context ctx);
    std::vector<domain::yield_curve_process_parameter_definition>
    read_latest(context ctx, const std::string& id);
    /**@}*/

    /**
     * @brief Reads all yield curve process parameter definitions, possibly filtered by primary key.
     */
    std::vector<domain::yield_curve_process_parameter_definition> read_all(context ctx,
                                                                           const std::string& id);

    /**
     * @brief Reads a single yield curve process parameter definition as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param version The version to fetch
     */
    std::optional<domain::yield_curve_process_parameter_definition>
    read_at_version(context ctx, const std::string& id, std::uint32_t version);

    /**
     * @brief Reads latest yield curve process parameter definitions with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::yield_curve_process_parameter_definition>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active yield curve process parameter definitions.
     * @param ctx Repository context with database connection
     * @return Total number of active yield curve process parameter definitions
     */
    std::uint32_t get_total_parameter_definition_count(context ctx);

    /**
     * @brief Deletes a yield curve process parameter definition by closing its temporal validity.
     */
    void remove(context ctx, const std::string& id);

    /**
     * @brief Deletes yield curve process parameter definitions by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);
};

}

#endif
