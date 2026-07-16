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
#ifndef ORES_REFDATA_CORE_SERVICE_BUSINESS_UNIT_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_BUSINESS_UNIT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/business_unit.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/business_unit_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing business units.
 *
 * Provides a higher-level interface for business unit operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT business_unit_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.business_unit_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a business_unit_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit business_unit_service(context ctx);

    /**
     * @brief Lists business units with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of business units for the requested page.
     */
    std::vector<domain::business_unit> list_business_units(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active business units.
     *
     * @return Total number of active business units.
     */
    std::uint32_t count_business_units();


    /**
     * @brief Retrieves a single business unit as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param id The id of the business unit.
     * @param version The version to fetch.
     * @return The business unit at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::business_unit> get_business_unit_at_version(const std::string& id,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single business unit by its id.
     *
     * @param id The id of the business unit.
     * @return The business unit if found, std::nullopt otherwise.
     */
    std::optional<domain::business_unit> get_business_unit(const std::string& id);

    /**
     * @brief Saves a business unit (creates or updates).
     *
     * @param business_unit The business unit to save.
     * @throws std::exception on failure.
     */
    void save_business_unit(const domain::business_unit& business_unit);

    /**
     * @brief Saves a batch of business units.
     *
     * @param business_units The business units to save.
     * @throws std::exception on failure.
     */
    void save_business_units(const std::vector<domain::business_unit>& business_units);

    /**
     * @brief Deletes a business unit by its id.
     *
     * @param id The id of the business unit to delete.
     * @throws std::exception on failure.
     */
    void delete_business_unit(const std::string& id);

    /**
     * @brief Deletes business units by their ids.
     */
    void delete_business_units(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a business unit.
     */
    std::vector<domain::business_unit> get_business_unit_history(const std::string& id);

private:
    context ctx_;
    repository::business_unit_repository repo_;
};

}

#endif
