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
#ifndef ORES_DQ_CORE_SERVICE_SYNTHETIC_FX_SPOT_CONFIG_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_SYNTHETIC_FX_SPOT_CONFIG_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/synthetic_fx_spot_config.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/synthetic_fx_spot_config_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing synthetic FX spot configs.
 *
 * Provides a higher-level interface for synthetic FX spot config operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT synthetic_fx_spot_config_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.synthetic_fx_spot_config_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a synthetic_fx_spot_config_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit synthetic_fx_spot_config_service(context ctx);

    /**
     * @brief Lists synthetic FX spot configs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of synthetic FX spot configs for the requested page.
     */
    std::vector<domain::synthetic_fx_spot_config> list_configs(std::uint32_t offset,
                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active synthetic FX spot configs.
     *
     * @return Total number of active synthetic FX spot configs.
     */
    std::uint32_t count_configs();


    /**
     * @brief Retrieves a single synthetic FX spot config as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The synthetic FX spot config at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::synthetic_fx_spot_config> get_config_at_version(const std::string& id,
                                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single synthetic FX spot config by its primary key.
     *
     * @return The synthetic FX spot config if found, std::nullopt otherwise.
     */
    std::optional<domain::synthetic_fx_spot_config> get_config(const std::string& id);

    /**
     * @brief Saves a synthetic FX spot config (creates or updates).
     *
     * @param config The synthetic FX spot config to save.
     * @throws std::exception on failure.
     */
    void save_config(const domain::synthetic_fx_spot_config& config);

    /**
     * @brief Saves a batch of synthetic FX spot configs.
     *
     * @param configs The synthetic FX spot configs to save.
     * @throws std::exception on failure.
     */
    void save_configs(const std::vector<domain::synthetic_fx_spot_config>& configs);

    /**
     * @brief Deletes a synthetic FX spot config by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_config(const std::string& id);

    /**
     * @brief Deletes synthetic FX spot configs by their primary keys.
     */
    void delete_configs(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a synthetic FX spot config.
     */
    std::vector<domain::synthetic_fx_spot_config> get_config_history(const std::string& id);

private:
    context ctx_;
    repository::synthetic_fx_spot_config_repository repo_;
};

}

#endif
