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
#ifndef ORES_REFDATA_CORE_SERVICE_BUSINESS_DAY_CONVENTION_TYPE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_BUSINESS_DAY_CONVENTION_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/business_day_convention_type.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/business_day_convention_type_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing business day convention types.
 *
 * Provides a higher-level interface for business day convention type operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT business_day_convention_type_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.business_day_convention_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a business_day_convention_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit business_day_convention_type_service(context ctx);

    /**
     * @brief Lists business day convention types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of business day convention types for the requested page.
     */
    std::vector<domain::business_day_convention_type> list_types(std::uint32_t offset,
                                                                 std::uint32_t limit);

    /**
     * @brief Gets the total count of active business day convention types.
     *
     * @return Total number of active business day convention types.
     */
    std::uint32_t count_types();


    /**
     * @brief Retrieves a single business day convention type by its code.
     *
     * @param code The code of the business day convention type.
     * @return The business day convention type if found, std::nullopt otherwise.
     */
    std::optional<domain::business_day_convention_type> get_type(const std::string& code);

    /**
     * @brief Saves a business day convention type (creates or updates).
     *
     * @param type The business day convention type to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::business_day_convention_type& type);

    /**
     * @brief Saves a batch of business day convention types.
     *
     * @param types The business day convention types to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::business_day_convention_type>& types);

    /**
     * @brief Deletes a business day convention type by its code.
     *
     * @param code The code of the business day convention type to delete.
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& code);

    /**
     * @brief Deletes business day convention types by their codes.
     */
    void delete_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a business day convention type.
     */
    std::vector<domain::business_day_convention_type> get_type_history(const std::string& code);

private:
    context ctx_;
    repository::business_day_convention_type_repository repo_;
};

}

#endif
