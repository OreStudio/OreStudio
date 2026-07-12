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
#ifndef ORES_MARKETDATA_CORE_SERVICE_CRM_TOPOLOGY_CONFIG_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_CRM_TOPOLOGY_CONFIG_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/crm_topology_config.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/crm_topology_config_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing CRM topology configs.
 *
 * Provides a higher-level interface for CRM topology config operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT crm_topology_config_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.crm_topology_config_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a crm_topology_config_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit crm_topology_config_service(context ctx);

    /**
     * @brief Lists CRM topology configs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of CRM topology configs for the requested page.
     */
    std::vector<domain::crm_topology_config> list_crm_topology_configs(std::uint32_t offset,
                                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active CRM topology configs.
     *
     * @return Total number of active CRM topology configs.
     */
    std::uint32_t count_crm_topology_configs();

    /**
     * @brief Retrieves a single CRM topology config as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param id The id of the CRM topology config.
     * @param version The version to fetch.
     * @return The CRM topology config at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::crm_topology_config>
    get_crm_topology_config_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single CRM topology config by its id.
     *
     * @param id The id of the CRM topology config.
     * @return The CRM topology config if found, std::nullopt otherwise.
     */
    std::optional<domain::crm_topology_config> get_crm_topology_config(const std::string& id);

    /**
     * @brief Saves a CRM topology config (creates or updates).
     *
     * @param crm_topology_config The CRM topology config to save.
     * @throws std::exception on failure.
     */
    void save_crm_topology_config(const domain::crm_topology_config& crm_topology_config);

    /**
     * @brief Saves a batch of CRM topology configs.
     *
     * @param crm_topology_configs The CRM topology configs to save.
     * @throws std::exception on failure.
     */
    void
    save_crm_topology_configs(const std::vector<domain::crm_topology_config>& crm_topology_configs);

    /**
     * @brief Deletes a CRM topology config by its id.
     *
     * @param id The id of the CRM topology config to delete.
     * @throws std::exception on failure.
     */
    void delete_crm_topology_config(const std::string& id);

    /**
     * @brief Deletes CRM topology configs by their ids.
     */
    void delete_crm_topology_configs(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a CRM topology config.
     */
    std::vector<domain::crm_topology_config> get_crm_topology_config_history(const std::string& id);

private:
    context ctx_;
    repository::crm_topology_config_repository repo_;
};

}

#endif
