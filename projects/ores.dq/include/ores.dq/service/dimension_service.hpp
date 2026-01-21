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
#ifndef ORES_DQ_SERVICE_DIMENSION_SERVICE_HPP
#define ORES_DQ_SERVICE_DIMENSION_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/nature_dimension.hpp"
#include "ores.dq/domain/origin_dimension.hpp"
#include "ores.dq/domain/treatment_dimension.hpp"
#include "ores.dq/repository/nature_dimension_repository.hpp"
#include "ores.dq/repository/origin_dimension_repository.hpp"
#include "ores.dq/repository/treatment_dimension_repository.hpp"

namespace ores::dq::service {

/**
 * @brief Service for managing dimensions (nature, origin, treatment).
 *
 * This service provides functionality for:
 * - Managing nature dimensions (CRUD operations)
 * - Managing origin dimensions (CRUD operations)
 * - Managing treatment dimensions (CRUD operations)
 */
class dimension_service {
private:
    inline static std::string_view logger_name =
        "ores.dq.service.dimension_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a dimension_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit dimension_service(context ctx);

    // ========================================================================
    // Nature Dimension Management
    // ========================================================================

    /**
     * @brief Lists all nature dimensions.
     */
    std::vector<domain::nature_dimension> list_nature_dimensions();

    /**
     * @brief Finds a nature dimension by its code.
     */
    std::optional<domain::nature_dimension>
    find_nature_dimension(const std::string& code);

    /**
     * @brief Saves a nature dimension (creates or updates).
     *
     * @param dimension The dimension to save
     */
    void save_nature_dimension(const domain::nature_dimension& dimension);

    /**
     * @brief Removes a nature dimension.
     *
     * @param code The code of the dimension to remove
     */
    void remove_nature_dimension(const std::string& code);

    /**
     * @brief Gets the version history for a nature dimension.
     *
     * @param code The dimension code
     * @return Vector of all versions, newest first
     */
    std::vector<domain::nature_dimension>
    get_nature_dimension_history(const std::string& code);

    // ========================================================================
    // Origin Dimension Management
    // ========================================================================

    /**
     * @brief Lists all origin dimensions.
     */
    std::vector<domain::origin_dimension> list_origin_dimensions();

    /**
     * @brief Finds an origin dimension by its code.
     */
    std::optional<domain::origin_dimension>
    find_origin_dimension(const std::string& code);

    /**
     * @brief Saves an origin dimension (creates or updates).
     *
     * @param dimension The dimension to save
     */
    void save_origin_dimension(const domain::origin_dimension& dimension);

    /**
     * @brief Removes an origin dimension.
     *
     * @param code The code of the dimension to remove
     */
    void remove_origin_dimension(const std::string& code);

    /**
     * @brief Gets the version history for an origin dimension.
     *
     * @param code The dimension code
     * @return Vector of all versions, newest first
     */
    std::vector<domain::origin_dimension>
    get_origin_dimension_history(const std::string& code);

    // ========================================================================
    // Treatment Dimension Management
    // ========================================================================

    /**
     * @brief Lists all treatment dimensions.
     */
    std::vector<domain::treatment_dimension> list_treatment_dimensions();

    /**
     * @brief Finds a treatment dimension by its code.
     */
    std::optional<domain::treatment_dimension>
    find_treatment_dimension(const std::string& code);

    /**
     * @brief Saves a treatment dimension (creates or updates).
     *
     * @param dimension The dimension to save
     */
    void save_treatment_dimension(const domain::treatment_dimension& dimension);

    /**
     * @brief Removes a treatment dimension.
     *
     * @param code The code of the dimension to remove
     */
    void remove_treatment_dimension(const std::string& code);

    /**
     * @brief Gets the version history for a treatment dimension.
     *
     * @param code The dimension code
     * @return Vector of all versions, newest first
     */
    std::vector<domain::treatment_dimension>
    get_treatment_dimension_history(const std::string& code);

private:
    repository::nature_dimension_repository nature_repo_;
    repository::origin_dimension_repository origin_repo_;
    repository::treatment_dimension_repository treatment_repo_;
};

}

#endif
