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
#ifndef ORES_MARKETDATA_CORE_SERVICE_MARKET_SERIES_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_MARKET_SERIES_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing market series.
 *
 * Provides a higher-level interface for market series operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT market_series_service {
private:
    inline static std::string_view logger_name = "ores.marketdata.service.market_series_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a market_series_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit market_series_service(context ctx);

    /**
     * @brief Lists market series with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of market series for the requested page.
     */
    std::vector<domain::market_series> list_market_series(std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of active market series.
     *
     * @return Total number of active market series.
     */
    std::uint32_t count_market_series();

    /**
     * @brief Retrieves a single market series by its id.
     *
     * @param id The id of the market series.
     * @return The market series if found, std::nullopt otherwise.
     */
    std::optional<domain::market_series> get_market_series(const std::string& id);

    /**
     * @brief Saves a market series (creates or updates).
     *
     * @param market_series The market series to save.
     * @throws std::exception on failure.
     */
    void save_market_series(const domain::market_series& market_series);

    /**
     * @brief Saves a batch of market series.
     *
     * @param market_series The market series to save.
     * @throws std::exception on failure.
     */
    void save_market_series(const std::vector<domain::market_series>& market_series);

    /**
     * @brief Deletes a market series by its id.
     *
     * @param id The id of the market series to delete.
     * @throws std::exception on failure.
     */
    void delete_market_series(const std::string& id);

    /**
     * @brief Deletes market series by their ids.
     */
    void delete_market_series(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a market series.
     */
    std::vector<domain::market_series> get_market_series_history(const std::string& id);

private:
    context ctx_;
    repository::market_series_repository repo_;
};

}

#endif
