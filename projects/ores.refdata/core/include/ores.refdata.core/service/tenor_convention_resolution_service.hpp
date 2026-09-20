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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_SERVICE_TENOR_CONVENTION_RESOLUTION_SERVICE_HPP
#define ORES_REFDATA_SERVICE_TENOR_CONVENTION_RESOLUTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
#include "ores.refdata.core/repository/tenor_convention_resolution_repository.hpp"
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing tenor convention resolutions.
 *
 * This service provides functionality for:
 * - Managing tenor convention resolutions (CRUD operations)
 */
class tenor_convention_resolution_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.tenor_convention_resolution_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenor_convention_resolution_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit tenor_convention_resolution_service(context ctx);

    /**
     * @brief Lists all tenor convention resolutions.
     */
    std::vector<domain::tenor_convention_resolution> list_resolutions();

    /**
     * @brief Lists tenor convention resolutions with pagination.
     */
    std::vector<domain::tenor_convention_resolution> list_resolutions(std::uint32_t offset,
                                                                      std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor convention resolutions.
     */
    std::uint32_t get_total_resolution_count();

    /**
     * @brief Lists tenor convention resolutions for a specific convention.
     *
     * @param convention_code The convention to filter by
     */
    std::vector<domain::tenor_convention_resolution>
    list_resolutions_by_convention(const std::string& convention_code);

    /**
     * @brief Lists tenor convention resolutions for a specific convention, with pagination.
     */
    std::vector<domain::tenor_convention_resolution> list_resolutions_by_convention(
        const std::string& convention_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor convention resolutions filtered by
     * convention_code.
     */
    std::uint32_t get_total_resolution_count_by_convention(const std::string& convention_code);

    /**
     * @brief Gets the total count of active tenor convention resolutions filtered by tenor_code.
     */
    std::uint32_t get_total_resolution_count_by_tenor(const std::string& tenor_code);

private:
    context ctx_;
    repository::tenor_convention_resolution_repository repo_;
};

}

#endif
