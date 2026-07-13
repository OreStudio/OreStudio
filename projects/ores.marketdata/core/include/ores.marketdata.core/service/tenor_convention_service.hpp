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
#ifndef ORES_MARKETDATA_CORE_SERVICE_TENOR_CONVENTION_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_TENOR_CONVENTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/tenor_convention.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/tenor_convention_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing tenor conventions.
 *
 * Provides a higher-level interface for tenor convention operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT tenor_convention_service {
private:
    inline static std::string_view logger_name = "ores.marketdata.service.tenor_convention_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenor_convention_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenor_convention_service(context ctx);

    /**
     * @brief Lists tenor conventions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of tenor conventions for the requested page.
     */
    std::vector<domain::tenor_convention> list_conventions(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor conventions.
     *
     * @return Total number of active tenor conventions.
     */
    std::uint32_t count_conventions();

    /**
     * @brief Retrieves a single tenor convention as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the tenor convention.
     * @param version The version to fetch.
     * @return The tenor convention at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_convention> get_convention_at_version(const std::string& code,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single tenor convention by its code.
     *
     * @param code The code of the tenor convention.
     * @return The tenor convention if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_convention> get_convention(const std::string& code);

    /**
     * @brief Saves a tenor convention (creates or updates).
     *
     * @param convention The tenor convention to save.
     * @throws std::exception on failure.
     */
    void save_convention(const domain::tenor_convention& convention);

    /**
     * @brief Saves a batch of tenor conventions.
     *
     * @param conventions The tenor conventions to save.
     * @throws std::exception on failure.
     */
    void save_conventions(const std::vector<domain::tenor_convention>& conventions);

    /**
     * @brief Deletes a tenor convention by its code.
     *
     * @param code The code of the tenor convention to delete.
     * @throws std::exception on failure.
     */
    void delete_convention(const std::string& code);

    /**
     * @brief Deletes tenor conventions by their codes.
     */
    void delete_conventions(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a tenor convention.
     */
    std::vector<domain::tenor_convention> get_convention_history(const std::string& code);

private:
    context ctx_;
    repository::tenor_convention_repository repo_;
};

}

#endif
