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
#ifndef ORES_TRADING_CORE_SERVICE_EQUITY_SWAP_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_EQUITY_SWAP_INSTRUMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/equity_swap_instrument.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/equity_swap_instrument_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing equity swap instruments.
 *
 * Provides a higher-level interface for equity swap instrument operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT equity_swap_instrument_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.equity_swap_instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a equity_swap_instrument_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit equity_swap_instrument_service(context ctx);

    /**
     * @brief Lists equity swap instruments with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of equity swap instruments for the requested page.
     */
    std::vector<domain::equity_swap_instrument> list_equity_swap_instruments(std::uint32_t offset,
                                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active equity swap instruments.
     *
     * @return Total number of active equity swap instruments.
     */
    std::uint32_t count_equity_swap_instruments();


    /**
     * @brief Retrieves a single equity swap instrument as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The equity swap instrument at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::equity_swap_instrument>
    get_equity_swap_instrument_at_version(const std::string& instrument_id, std::uint32_t version);

    /**
     * @brief Retrieves a single equity swap instrument by its primary key.
     *
     * @return The equity swap instrument if found, std::nullopt otherwise.
     */
    std::optional<domain::equity_swap_instrument>
    get_equity_swap_instrument(const std::string& instrument_id);

    /**
     * @brief Retrieves a batch of equity swap instruments by primary key.
     */
    std::vector<domain::equity_swap_instrument>
    get_equity_swap_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Saves a equity swap instrument (creates or updates).
     *
     * @param equity_swap_instrument The equity swap instrument to save.
     * @throws std::exception on failure.
     */
    void save_equity_swap_instrument(const domain::equity_swap_instrument& equity_swap_instrument);

    /**
     * @brief Saves a batch of equity swap instruments.
     *
     * @param equity_swap_instruments The equity swap instruments to save.
     * @throws std::exception on failure.
     */
    void save_equity_swap_instruments(
        const std::vector<domain::equity_swap_instrument>& equity_swap_instruments);

    /**
     * @brief Deletes a equity swap instrument by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_equity_swap_instrument(const std::string& instrument_id);

    /**
     * @brief Deletes equity swap instruments by their primary keys.
     */
    void delete_equity_swap_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a equity swap instrument.
     */
    std::vector<domain::equity_swap_instrument>
    get_equity_swap_instrument_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::equity_swap_instrument_repository repo_;
};

}

#endif
