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
#ifndef ORES_REFDATA_SERVICE_BUSINESS_UNIT_SERVICE_HPP
#define ORES_REFDATA_SERVICE_BUSINESS_UNIT_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/business_unit.hpp"
#include "ores.refdata/repository/business_unit_repository.hpp"

namespace ores::refdata::service {

/**
 * @brief Service for managing business units.
 *
 * This service provides functionality for:
 * - Managing business units (CRUD operations)
 */
class business_unit_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.business_unit_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a business_unit_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit business_unit_service(context ctx);

    /**
     * @brief Lists all business units.
     */
    std::vector<domain::business_unit> list_business_units();

    /**
     * @brief Finds a business unit by its ID.
     */
    std::optional<domain::business_unit>
    find_business_unit(const boost::uuids::uuid& id);

    /**
     * @brief Finds a business unit by its code.
     */
    std::optional<domain::business_unit>
    find_business_unit_by_code(const std::string& code);

    /**
     * @brief Saves a business unit (creates or updates).
     *
     * @param business_unit The business unit to save
     */
    void save_business_unit(const domain::business_unit& business_unit);

    /**
     * @brief Saves multiple business units (creates or updates).
     *
     * @param business_units The business units to save
     */
    void save_business_units(const std::vector<domain::business_unit>& business_units);

    /**
     * @brief Removes a business unit.
     *
     * @param id The ID of the business unit to remove
     */
    void remove_business_unit(const boost::uuids::uuid& id);

    /**
     * @brief Gets the version history for a business unit.
     *
     * @param id The business unit ID
     * @return Vector of all versions, newest first
     */
    std::vector<domain::business_unit>
    get_business_unit_history(const boost::uuids::uuid& id);

private:
    repository::business_unit_repository repo_;
};

}

#endif
