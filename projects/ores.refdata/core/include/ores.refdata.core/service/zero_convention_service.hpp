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
#ifndef ORES_REFDATA_CORE_SERVICE_ZERO_CONVENTION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_ZERO_CONVENTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/zero_convention.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/zero_convention_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing zero conventions.
 *
 * Provides a higher-level interface for zero convention operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT zero_convention_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.zero_convention_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a zero_convention_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit zero_convention_service(context ctx);

    /**
     * @brief Lists zero conventions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of zero conventions for the requested page.
     */
    std::vector<domain::zero_convention> list_zero_conventions(std::uint32_t offset,
                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active zero conventions.
     *
     * @return Total number of active zero conventions.
     */
    std::uint32_t count_zero_conventions();


    /**
     * @brief Retrieves a single zero convention as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The zero convention at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::zero_convention> get_zero_convention_at_version(const std::string& id,
                                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single zero convention by its primary key.
     *
     * @return The zero convention if found, std::nullopt otherwise.
     */
    std::optional<domain::zero_convention> get_zero_convention(const std::string& id);

    /**
     * @brief Saves a zero convention (creates or updates).
     *
     * @param zero_convention The zero convention to save.
     * @throws std::exception on failure.
     */
    void save_zero_convention(const domain::zero_convention& zero_convention);

    /**
     * @brief Saves a batch of zero conventions.
     *
     * @param zero_conventions The zero conventions to save.
     * @throws std::exception on failure.
     */
    void save_zero_conventions(const std::vector<domain::zero_convention>& zero_conventions);

    /**
     * @brief Deletes a zero convention by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_zero_convention(const std::string& id);

    /**
     * @brief Deletes zero conventions by their primary keys.
     */
    void delete_zero_conventions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a zero convention.
     */
    std::vector<domain::zero_convention> get_zero_convention_history(const std::string& id);

private:
    context ctx_;
    repository::zero_convention_repository repo_;
};

}

#endif
