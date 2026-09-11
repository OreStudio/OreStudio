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
#ifndef ORES_TRADING_CORE_SERVICE_INSTRUMENT_STRIKE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_INSTRUMENT_STRIKE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/instrument_strike.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/instrument_strike_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing instrument strikes.
 *
 * Provides a higher-level interface for instrument strike operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT instrument_strike_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.instrument_strike_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a instrument_strike_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit instrument_strike_service(context ctx);

    /**
     * @brief Lists instrument strikes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of instrument strikes for the requested page.
     */
    std::vector<domain::instrument_strike> list_instrument_strikes(std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active instrument strikes.
     *
     * @return Total number of active instrument strikes.
     */
    std::uint32_t count_instrument_strikes();


    /**
     * @brief Retrieves a single instrument strike as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The instrument strike at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_strike>
    get_instrument_strike_at_version(const std::string& instrument_id, std::uint32_t version);

    /**
     * @brief Retrieves a single instrument strike by its primary key.
     *
     * @return The instrument strike if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_strike>
    get_instrument_strike(const std::string& instrument_id);

    /**
     * @brief Saves a instrument strike (creates or updates).
     *
     * @param instrument_strike The instrument strike to save.
     * @throws std::exception on failure.
     */
    void save_instrument_strike(const domain::instrument_strike& instrument_strike);

    /**
     * @brief Saves a batch of instrument strikes.
     *
     * @param instrument_strikes The instrument strikes to save.
     * @throws std::exception on failure.
     */
    void save_instrument_strikes(const std::vector<domain::instrument_strike>& instrument_strikes);

    /**
     * @brief Deletes a instrument strike by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_instrument_strike(const std::string& instrument_id);

    /**
     * @brief Deletes instrument strikes by their primary keys.
     */
    void delete_instrument_strikes(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a instrument strike.
     */
    std::vector<domain::instrument_strike>
    get_instrument_strike_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::instrument_strike_repository repo_;
};

}

#endif
