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
#ifndef ORES_TRADING_CORE_SERVICE_SWAPTION_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_SWAPTION_INSTRUMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/swaption_instrument.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/swaption_instrument_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing swaption instruments.
 *
 * Provides a higher-level interface for swaption instrument operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT swaption_instrument_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.swaption_instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a swaption_instrument_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit swaption_instrument_service(context ctx);

    /**
     * @brief Lists swaption instruments with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of swaption instruments for the requested page.
     */
    std::vector<domain::swaption_instrument> list_swaption_instruments(std::uint32_t offset,
                                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active swaption instruments.
     *
     * @return Total number of active swaption instruments.
     */
    std::uint32_t count_swaption_instruments();


    /**
     * @brief Retrieves a single swaption instrument as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The swaption instrument at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::swaption_instrument>
    get_swaption_instrument_at_version(const std::string& instrument_id, std::uint32_t version);

    /**
     * @brief Retrieves a single swaption instrument by its primary key.
     *
     * @return The swaption instrument if found, std::nullopt otherwise.
     */
    std::optional<domain::swaption_instrument>
    get_swaption_instrument(const std::string& instrument_id);

    /**
     * @brief Retrieves a batch of swaption instruments by primary key.
     */
    std::vector<domain::swaption_instrument>
    get_swaption_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Saves a swaption instrument (creates or updates).
     *
     * @param swaption_instrument The swaption instrument to save.
     * @throws std::exception on failure.
     */
    void save_swaption_instrument(const domain::swaption_instrument& swaption_instrument);

    /**
     * @brief Saves a batch of swaption instruments.
     *
     * @param swaption_instruments The swaption instruments to save.
     * @throws std::exception on failure.
     */
    void
    save_swaption_instruments(const std::vector<domain::swaption_instrument>& swaption_instruments);

    /**
     * @brief Deletes a swaption instrument by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_swaption_instrument(const std::string& instrument_id);

    /**
     * @brief Deletes swaption instruments by their primary keys.
     */
    void delete_swaption_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a swaption instrument.
     */
    std::vector<domain::swaption_instrument>
    get_swaption_instrument_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::swaption_instrument_repository repo_;
};

}

#endif
