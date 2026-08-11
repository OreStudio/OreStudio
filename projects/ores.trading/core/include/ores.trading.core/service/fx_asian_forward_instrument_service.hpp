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
#ifndef ORES_TRADING_CORE_SERVICE_FX_ASIAN_FORWARD_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_FX_ASIAN_FORWARD_INSTRUMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/fx_asian_forward_instrument.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/fx_asian_forward_instrument_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing FX Asian Forward instruments.
 *
 * Provides a higher-level interface for FX Asian Forward instrument operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT fx_asian_forward_instrument_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.fx_asian_forward_instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a fx_asian_forward_instrument_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit fx_asian_forward_instrument_service(context ctx);

    /**
     * @brief Lists FX Asian Forward instruments with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of FX Asian Forward instruments for the requested page.
     */
    std::vector<domain::fx_asian_forward_instrument>
    list_fx_asian_forward_instruments(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active FX Asian Forward instruments.
     *
     * @return Total number of active FX Asian Forward instruments.
     */
    std::uint32_t count_fx_asian_forward_instruments();


    /**
     * @brief Retrieves a single FX Asian Forward instrument as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The FX Asian Forward instrument at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::fx_asian_forward_instrument>
    get_fx_asian_forward_instrument_at_version(const std::string& instrument_id,
                                               std::uint32_t version);

    /**
     * @brief Retrieves a single FX Asian Forward instrument by its primary key.
     *
     * @return The FX Asian Forward instrument if found, std::nullopt otherwise.
     */
    std::optional<domain::fx_asian_forward_instrument>
    get_fx_asian_forward_instrument(const std::string& instrument_id);

    /**
     * @brief Retrieves a batch of FX Asian Forward instruments by primary key.
     */
    std::vector<domain::fx_asian_forward_instrument>
    get_fx_asian_forward_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Saves a FX Asian Forward instrument (creates or updates).
     *
     * @param fx_asian_forward_instrument The FX Asian Forward instrument to save.
     * @throws std::exception on failure.
     */
    void save_fx_asian_forward_instrument(
        const domain::fx_asian_forward_instrument& fx_asian_forward_instrument);

    /**
     * @brief Saves a batch of FX Asian Forward instruments.
     *
     * @param fx_asian_forward_instruments The FX Asian Forward instruments to save.
     * @throws std::exception on failure.
     */
    void save_fx_asian_forward_instruments(
        const std::vector<domain::fx_asian_forward_instrument>& fx_asian_forward_instruments);

    /**
     * @brief Deletes a FX Asian Forward instrument by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_fx_asian_forward_instrument(const std::string& instrument_id);

    /**
     * @brief Deletes FX Asian Forward instruments by their primary keys.
     */
    void delete_fx_asian_forward_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a FX Asian Forward instrument.
     */
    std::vector<domain::fx_asian_forward_instrument>
    get_fx_asian_forward_instrument_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::fx_asian_forward_instrument_repository repo_;
};

}

#endif
