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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_MARKET_DATA_GENERATION_CONFIG_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_MARKET_DATA_GENERATION_CONFIG_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/market_data_generation_config.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/market_data_generation_config_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing market data generation configs.
 *
 * Provides a higher-level interface for market data generation config operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT market_data_generation_config_service {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.service.market_data_generation_config_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a market_data_generation_config_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit market_data_generation_config_service(context ctx);

    /**
     * @brief Lists market data generation configs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of market data generation configs for the requested page.
     */
    std::vector<domain::market_data_generation_config>
    list_market_data_generation_configs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active market data generation configs.
     *
     * @return Total number of active market data generation configs.
     */
    std::uint32_t count_market_data_generation_configs();

    /**
     * @brief Retrieves a single market data generation config by its id.
     *
     * @param id The id of the market data generation config.
     * @return The market data generation config if found, std::nullopt otherwise.
     */
    std::optional<domain::market_data_generation_config>
    get_market_data_generation_config(const std::string& id);

    /**
     * @brief Saves a market data generation config (creates or updates).
     *
     * @param market_data_generation_config The market data generation config to save.
     * @throws std::exception on failure.
     */
    void save_market_data_generation_config(
        const domain::market_data_generation_config& market_data_generation_config);

    /**
     * @brief Saves a batch of market data generation configs.
     *
     * @param market_data_generation_configs The market data generation configs to save.
     * @throws std::exception on failure.
     */
    void save_market_data_generation_configs(
        const std::vector<domain::market_data_generation_config>& market_data_generation_configs);

    /**
     * @brief Deletes a market data generation config by its id.
     *
     * @param id The id of the market data generation config to delete.
     * @throws std::exception on failure.
     */
    void delete_market_data_generation_config(const std::string& id);

    /**
     * @brief Deletes market data generation configs by their ids.
     */
    void delete_market_data_generation_configs(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a market data generation config.
     */
    std::vector<domain::market_data_generation_config>
    get_market_data_generation_config_history(const std::string& id);

private:
    context ctx_;
    repository::market_data_generation_config_repository repo_;
};

}

#endif
