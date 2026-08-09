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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_YIELD_CURVE_PROCESS_PARAMETER_DEFINITION_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_YIELD_CURVE_PROCESS_PARAMETER_DEFINITION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/yield_curve_process_parameter_definition_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing yield curve process parameter definitions.
 *
 * Provides a higher-level interface for yield curve process parameter definition operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT yield_curve_process_parameter_definition_service {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.service.yield_curve_process_parameter_definition_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a yield_curve_process_parameter_definition_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit yield_curve_process_parameter_definition_service(context ctx);

    /**
     * @brief Lists yield curve process parameter definitions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of yield curve process parameter definitions for the requested page.
     */
    std::vector<domain::yield_curve_process_parameter_definition>
    list_parameter_definitions(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active yield curve process parameter definitions.
     *
     * @return Total number of active yield curve process parameter definitions.
     */
    std::uint32_t count_parameter_definitions();


    /**
     * @brief Retrieves a single yield curve process parameter definition as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The yield curve process parameter definition at that version if found, std::nullopt
     * otherwise.
     */
    std::optional<domain::yield_curve_process_parameter_definition>
    get_parameter_definition_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single yield curve process parameter definition by its primary key.
     *
     * @return The yield curve process parameter definition if found, std::nullopt otherwise.
     */
    std::optional<domain::yield_curve_process_parameter_definition>
    get_parameter_definition(const std::string& id);

    /**
     * @brief Saves a yield curve process parameter definition (creates or updates).
     *
     * @param parameter_definition The yield curve process parameter definition to save.
     * @throws std::exception on failure.
     */
    void save_parameter_definition(
        const domain::yield_curve_process_parameter_definition& parameter_definition);

    /**
     * @brief Saves a batch of yield curve process parameter definitions.
     *
     * @param parameter_definitions The yield curve process parameter definitions to save.
     * @throws std::exception on failure.
     */
    void save_parameter_definitions(
        const std::vector<domain::yield_curve_process_parameter_definition>& parameter_definitions);

    /**
     * @brief Deletes a yield curve process parameter definition by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_parameter_definition(const std::string& id);

    /**
     * @brief Deletes yield curve process parameter definitions by their primary keys.
     */
    void delete_parameter_definitions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a yield curve process parameter definition.
     */
    std::vector<domain::yield_curve_process_parameter_definition>
    get_parameter_definition_history(const std::string& id);

private:
    context ctx_;
    repository::yield_curve_process_parameter_definition_repository repo_;
};

}

#endif
