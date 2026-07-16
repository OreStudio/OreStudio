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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_YIELD_CURVE_PROCESS_TYPE_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_YIELD_CURVE_PROCESS_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_type.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/yield_curve_process_type_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing yield curve process types.
 *
 * Provides a higher-level interface for yield curve process type operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT yield_curve_process_type_service {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.service.yield_curve_process_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a yield_curve_process_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit yield_curve_process_type_service(context ctx);

    /**
     * @brief Lists yield curve process types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of yield curve process types for the requested page.
     */
    std::vector<domain::yield_curve_process_type> list_process_types(std::uint32_t offset,
                                                                     std::uint32_t limit);

    /**
     * @brief Gets the total count of active yield curve process types.
     *
     * @return Total number of active yield curve process types.
     */
    std::uint32_t count_process_types();

    /**
     * @brief Retrieves a single yield curve process type as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the yield curve process type.
     * @param version The version to fetch.
     * @return The yield curve process type at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::yield_curve_process_type>
    get_process_type_at_version(const std::string& code, std::uint32_t version);

    /**
     * @brief Retrieves a single yield curve process type by its code.
     *
     * @param code The code of the yield curve process type.
     * @return The yield curve process type if found, std::nullopt otherwise.
     */
    std::optional<domain::yield_curve_process_type> get_process_type(const std::string& code);

    /**
     * @brief Saves a yield curve process type (creates or updates).
     *
     * @param process_type The yield curve process type to save.
     * @throws std::exception on failure.
     */
    void save_process_type(const domain::yield_curve_process_type& process_type);

    /**
     * @brief Saves a batch of yield curve process types.
     *
     * @param process_types The yield curve process types to save.
     * @throws std::exception on failure.
     */
    void save_process_types(const std::vector<domain::yield_curve_process_type>& process_types);

    /**
     * @brief Deletes a yield curve process type by its code.
     *
     * @param code The code of the yield curve process type to delete.
     * @throws std::exception on failure.
     */
    void delete_process_type(const std::string& code);

    /**
     * @brief Deletes yield curve process types by their codes.
     */
    void delete_process_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a yield curve process type.
     */
    std::vector<domain::yield_curve_process_type> get_process_type_history(const std::string& code);

private:
    context ctx_;
    repository::yield_curve_process_type_repository repo_;
};

}

#endif
