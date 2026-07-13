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
#ifndef ORES_REFDATA_CORE_SERVICE_TENOR_RESOLUTION_ALGORITHM_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_TENOR_RESOLUTION_ALGORITHM_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/tenor_resolution_algorithm.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/tenor_resolution_algorithm_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing tenor resolution algorithms.
 *
 * Provides a higher-level interface for tenor resolution algorithm operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT tenor_resolution_algorithm_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.tenor_resolution_algorithm_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenor_resolution_algorithm_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenor_resolution_algorithm_service(context ctx);

    /**
     * @brief Lists tenor resolution algorithms with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of tenor resolution algorithms for the requested page.
     */
    std::vector<domain::tenor_resolution_algorithm> list_algorithms(std::uint32_t offset,
                                                                    std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor resolution algorithms.
     *
     * @return Total number of active tenor resolution algorithms.
     */
    std::uint32_t count_algorithms();

    /**
     * @brief Retrieves a single tenor resolution algorithm as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the tenor resolution algorithm.
     * @param version The version to fetch.
     * @return The tenor resolution algorithm at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_resolution_algorithm>
    get_algorithm_at_version(const std::string& code, std::uint32_t version);

    /**
     * @brief Retrieves a single tenor resolution algorithm by its code.
     *
     * @param code The code of the tenor resolution algorithm.
     * @return The tenor resolution algorithm if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_resolution_algorithm> get_algorithm(const std::string& code);

    /**
     * @brief Saves a tenor resolution algorithm (creates or updates).
     *
     * @param algorithm The tenor resolution algorithm to save.
     * @throws std::exception on failure.
     */
    void save_algorithm(const domain::tenor_resolution_algorithm& algorithm);

    /**
     * @brief Saves a batch of tenor resolution algorithms.
     *
     * @param algorithms The tenor resolution algorithms to save.
     * @throws std::exception on failure.
     */
    void save_algorithms(const std::vector<domain::tenor_resolution_algorithm>& algorithms);

    /**
     * @brief Deletes a tenor resolution algorithm by its code.
     *
     * @param code The code of the tenor resolution algorithm to delete.
     * @throws std::exception on failure.
     */
    void delete_algorithm(const std::string& code);

    /**
     * @brief Deletes tenor resolution algorithms by their codes.
     */
    void delete_algorithms(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a tenor resolution algorithm.
     */
    std::vector<domain::tenor_resolution_algorithm> get_algorithm_history(const std::string& code);

private:
    context ctx_;
    repository::tenor_resolution_algorithm_repository repo_;
};

}

#endif
