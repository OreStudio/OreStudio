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
#ifndef ORES_TRADING_CORE_SERVICE_RPA_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_RPA_INSTRUMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/rpa_instrument.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/rpa_instrument_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing RPA instruments.
 *
 * Provides a higher-level interface for RPA instrument operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT rpa_instrument_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.rpa_instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a rpa_instrument_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit rpa_instrument_service(context ctx);

    /**
     * @brief Lists RPA instruments with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of RPA instruments for the requested page.
     */
    std::vector<domain::rpa_instrument> list_rpa_instruments(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active RPA instruments.
     *
     * @return Total number of active RPA instruments.
     */
    std::uint32_t count_rpa_instruments();


    /**
     * @brief Retrieves a single RPA instrument as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The RPA instrument at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::rpa_instrument>
    get_rpa_instrument_at_version(const std::string& instrument_id, std::uint32_t version);

    /**
     * @brief Retrieves a single RPA instrument by its primary key.
     *
     * @return The RPA instrument if found, std::nullopt otherwise.
     */
    std::optional<domain::rpa_instrument> get_rpa_instrument(const std::string& instrument_id);

    /**
     * @brief Retrieves a batch of RPA instruments by primary key.
     */
    std::vector<domain::rpa_instrument>
    get_rpa_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Saves a RPA instrument (creates or updates).
     *
     * @param rpa_instrument The RPA instrument to save.
     * @throws std::exception on failure.
     */
    void save_rpa_instrument(const domain::rpa_instrument& rpa_instrument);

    /**
     * @brief Saves a batch of RPA instruments.
     *
     * @param rpa_instruments The RPA instruments to save.
     * @throws std::exception on failure.
     */
    void save_rpa_instruments(const std::vector<domain::rpa_instrument>& rpa_instruments);

    /**
     * @brief Deletes a RPA instrument by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_rpa_instrument(const std::string& instrument_id);

    /**
     * @brief Deletes RPA instruments by their primary keys.
     */
    void delete_rpa_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a RPA instrument.
     */
    std::vector<domain::rpa_instrument>
    get_rpa_instrument_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::rpa_instrument_repository repo_;
};

}

#endif
