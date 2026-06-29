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
 * Provides a higher-level interface for config operations, wrapping the
 * underlying repository.
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

    explicit market_data_generation_config_service(context ctx);

    /**
     * @brief Lists configs with pagination support.
     */
    std::vector<domain::market_data_generation_config> list_configs(std::uint32_t offset,
                                                                    std::uint32_t limit);

    /**
     * @brief Gets the total count of active configs.
     */
    std::uint32_t count_configs();

    /**
     * @brief Saves a config (creates or updates).
     */
    void save_config(const domain::market_data_generation_config& config);

    /**
     * @brief Saves a batch of configs atomically (all or nothing).
     */
    void save_configs(const std::vector<domain::market_data_generation_config>& configs);

    /**
     * @brief Deletes a config by its id.
     */
    void delete_config(const std::string& id);

    /**
     * @brief Deletes configs by their ids.
     */
    void delete_configs(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves a single config by its id.
     */
    std::optional<domain::market_data_generation_config> get_config(const std::string& id);

    /**
     * @brief Retrieves all historical versions of a config.
     */
    std::vector<domain::market_data_generation_config> get_config_history(const std::string& id);

private:
    context ctx_;
    repository::market_data_generation_config_repository repo_;
};

}

#endif
