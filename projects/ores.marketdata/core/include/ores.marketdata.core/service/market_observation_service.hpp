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
#ifndef ORES_MARKETDATA_CORE_SERVICE_MARKET_OBSERVATION_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_MARKET_OBSERVATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing market observations.
 *
 * Provides a higher-level interface for market observation operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT market_observation_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.market_observation_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a market_observation_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit market_observation_service(context ctx);

    /**
     * @brief Lists market observations with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of market observations for the requested page.
     */
    std::vector<domain::market_observation> list_market_observations(std::uint32_t offset,
                                                                     std::uint32_t limit,
                                                                     const std::string& series_id = {});

    /**
     * @brief Gets the total count of active market observations.
     *
     * @return Total number of active market observations.
     */
    std::uint32_t count_market_observations();

    /**
     * @brief Retrieves a single market observation by its id.
     *
     * @param id The id of the market observation.
     * @return The market observation if found, std::nullopt otherwise.
     */
    std::optional<domain::market_observation> get_market_observation(const std::string& id);

    /**
     * @brief Saves a market observation (creates or updates).
     *
     * @param market_observation The market observation to save.
     * @throws std::exception on failure.
     */
    void save_market_observation(const domain::market_observation& market_observation);

    /**
     * @brief Saves a batch of market observations.
     *
     * @param market_observations The market observations to save.
     * @throws std::exception on failure.
     */
    void
    save_market_observations(const std::vector<domain::market_observation>& market_observations);

    /**
     * @brief Deletes a market observation by its id.
     *
     * @param id The id of the market observation to delete.
     * @throws std::exception on failure.
     */
    void delete_market_observation(const std::string& id);

    /**
     * @brief Deletes market observations by their ids.
     */
    void delete_market_observations(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a market observation.
     */
    std::vector<domain::market_observation> get_market_observation_history(const std::string& id);

private:
    context ctx_;
    repository::market_observation_repository repo_;
};

}

#endif
