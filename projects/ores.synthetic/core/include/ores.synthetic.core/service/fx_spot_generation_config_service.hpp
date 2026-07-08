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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_FX_SPOT_GENERATION_CONFIG_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_FX_SPOT_GENERATION_CONFIG_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/fx_spot_generation_config_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing FX spot generation configs.
 *
 * Provides a higher-level interface for FX spot generation config operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT fx_spot_generation_config_service {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.service.fx_spot_generation_config_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a fx_spot_generation_config_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit fx_spot_generation_config_service(context ctx);

    /**
     * @brief Lists FX spot generation configs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of FX spot generation configs for the requested page.
     */
    std::vector<domain::fx_spot_generation_config>
    list_fx_spot_generation_configs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active FX spot generation configs.
     *
     * @return Total number of active FX spot generation configs.
     */
    std::uint32_t count_fx_spot_generation_configs();


    /**
     * @brief Retrieves a single FX spot generation config by its id.
     *
     * @param id The id of the FX spot generation config.
     * @return The FX spot generation config if found, std::nullopt otherwise.
     */
    std::optional<domain::fx_spot_generation_config>
    get_fx_spot_generation_config(const std::string& id);

    /**
     * @brief Saves a FX spot generation config (creates or updates).
     *
     * @param fx_spot_generation_config The FX spot generation config to save.
     * @throws std::exception on failure.
     */
    void save_fx_spot_generation_config(
        const domain::fx_spot_generation_config& fx_spot_generation_config);

    /**
     * @brief Saves a batch of FX spot generation configs.
     *
     * @param fx_spot_generation_configs The FX spot generation configs to save.
     * @throws std::exception on failure.
     */
    void save_fx_spot_generation_configs(
        const std::vector<domain::fx_spot_generation_config>& fx_spot_generation_configs);

    /**
     * @brief Deletes a FX spot generation config by its id.
     *
     * @param id The id of the FX spot generation config to delete.
     * @throws std::exception on failure.
     */
    void delete_fx_spot_generation_config(const std::string& id);

    /**
     * @brief Deletes FX spot generation configs by their ids.
     */
    void delete_fx_spot_generation_configs(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a FX spot generation config.
     */
    std::vector<domain::fx_spot_generation_config>
    get_fx_spot_generation_config_history(const std::string& id);

private:
    context ctx_;
    repository::fx_spot_generation_config_repository repo_;
};

}

#endif
