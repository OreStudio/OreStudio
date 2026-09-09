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
#ifndef ORES_TRADING_CORE_SERVICE_ASCOT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_ASCOT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/ascot.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/ascot_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing ascots.
 *
 * Provides a higher-level interface for ascot operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT ascot_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.ascot_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a ascot_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit ascot_service(context ctx);

    /**
     * @brief Lists ascots with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of ascots for the requested page.
     */
    std::vector<domain::ascot> list_ascots(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active ascots.
     *
     * @return Total number of active ascots.
     */
    std::uint32_t count_ascots();


    /**
     * @brief Retrieves a single ascot as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The ascot at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::ascot> get_ascot_at_version(const std::string& instrument_id,
                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single ascot by its primary key.
     *
     * @return The ascot if found, std::nullopt otherwise.
     */
    std::optional<domain::ascot> get_ascot(const std::string& instrument_id);

    /**
     * @brief Saves a ascot (creates or updates).
     *
     * @param ascot The ascot to save.
     * @throws std::exception on failure.
     */
    void save_ascot(const domain::ascot& ascot);

    /**
     * @brief Saves a batch of ascots.
     *
     * @param ascots The ascots to save.
     * @throws std::exception on failure.
     */
    void save_ascots(const std::vector<domain::ascot>& ascots);

    /**
     * @brief Deletes a ascot by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_ascot(const std::string& instrument_id);

    /**
     * @brief Deletes ascots by their primary keys.
     */
    void delete_ascots(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a ascot.
     */
    std::vector<domain::ascot> get_ascot_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::ascot_repository repo_;
};

}

#endif
