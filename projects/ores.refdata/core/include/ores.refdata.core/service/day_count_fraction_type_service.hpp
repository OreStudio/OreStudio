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
#ifndef ORES_REFDATA_CORE_SERVICE_DAY_COUNT_FRACTION_TYPE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_DAY_COUNT_FRACTION_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/day_count_fraction_type.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/day_count_fraction_type_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing day count fraction types.
 *
 * Provides a higher-level interface for day count fraction type operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT day_count_fraction_type_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.day_count_fraction_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a day_count_fraction_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit day_count_fraction_type_service(context ctx);

    /**
     * @brief Lists day count fraction types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of day count fraction types for the requested page.
     */
    std::vector<domain::day_count_fraction_type> list_types(std::uint32_t offset,
                                                            std::uint32_t limit);

    /**
     * @brief Gets the total count of active day count fraction types.
     *
     * @return Total number of active day count fraction types.
     */
    std::uint32_t count_types();

    /**
     * @brief Retrieves a single day count fraction type as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the day count fraction type.
     * @param version The version to fetch.
     * @return The day count fraction type at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::day_count_fraction_type> get_type_at_version(const std::string& code,
                                                                       std::uint32_t version);

    /**
     * @brief Retrieves a single day count fraction type by its code.
     *
     * @param code The code of the day count fraction type.
     * @return The day count fraction type if found, std::nullopt otherwise.
     */
    std::optional<domain::day_count_fraction_type> get_type(const std::string& code);

    /**
     * @brief Saves a day count fraction type (creates or updates).
     *
     * @param type The day count fraction type to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::day_count_fraction_type& type);

    /**
     * @brief Saves a batch of day count fraction types.
     *
     * @param types The day count fraction types to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::day_count_fraction_type>& types);

    /**
     * @brief Deletes a day count fraction type by its code.
     *
     * @param code The code of the day count fraction type to delete.
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& code);

    /**
     * @brief Deletes day count fraction types by their codes.
     */
    void delete_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a day count fraction type.
     */
    std::vector<domain::day_count_fraction_type> get_type_history(const std::string& code);

private:
    context ctx_;
    repository::day_count_fraction_type_repository repo_;
};

}

#endif
