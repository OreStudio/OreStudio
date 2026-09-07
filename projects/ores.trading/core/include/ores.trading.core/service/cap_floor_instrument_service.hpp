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
#ifndef ORES_TRADING_CORE_SERVICE_CAP_FLOOR_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_CAP_FLOOR_INSTRUMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/cap_floor_instrument.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/cap_floor_instrument_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing cap/floor instruments.
 *
 * Provides a higher-level interface for cap/floor instrument operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT cap_floor_instrument_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.cap_floor_instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a cap_floor_instrument_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit cap_floor_instrument_service(context ctx);

    /**
     * @brief Lists cap/floor instruments with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of cap/floor instruments for the requested page.
     */
    std::vector<domain::cap_floor_instrument> list_cap_floor_instruments(std::uint32_t offset,
                                                                         std::uint32_t limit);

    /**
     * @brief Gets the total count of active cap/floor instruments.
     *
     * @return Total number of active cap/floor instruments.
     */
    std::uint32_t count_cap_floor_instruments();


    /**
     * @brief Retrieves a single cap/floor instrument as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The cap/floor instrument at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::cap_floor_instrument>
    get_cap_floor_instrument_at_version(const std::string& instrument_id, std::uint32_t version);

    /**
     * @brief Retrieves a single cap/floor instrument by its primary key.
     *
     * @return The cap/floor instrument if found, std::nullopt otherwise.
     */
    std::optional<domain::cap_floor_instrument>
    get_cap_floor_instrument(const std::string& instrument_id);

    /**
     * @brief Retrieves a batch of cap/floor instruments by primary key.
     */
    std::vector<domain::cap_floor_instrument>
    get_cap_floor_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Saves a cap/floor instrument (creates or updates).
     *
     * @param cap_floor_instrument The cap/floor instrument to save.
     * @throws std::exception on failure.
     */
    void save_cap_floor_instrument(const domain::cap_floor_instrument& cap_floor_instrument);

    /**
     * @brief Saves a batch of cap/floor instruments.
     *
     * @param cap_floor_instruments The cap/floor instruments to save.
     * @throws std::exception on failure.
     */
    void save_cap_floor_instruments(
        const std::vector<domain::cap_floor_instrument>& cap_floor_instruments);

    /**
     * @brief Deletes a cap/floor instrument by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_cap_floor_instrument(const std::string& instrument_id);

    /**
     * @brief Deletes cap/floor instruments by their primary keys.
     */
    void delete_cap_floor_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a cap/floor instrument.
     */
    std::vector<domain::cap_floor_instrument>
    get_cap_floor_instrument_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::cap_floor_instrument_repository repo_;
};

}

#endif
