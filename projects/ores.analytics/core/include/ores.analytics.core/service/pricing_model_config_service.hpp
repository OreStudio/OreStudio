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
#ifndef ORES_ANALYTICS_CORE_SERVICE_PRICING_MODEL_CONFIG_SERVICE_HPP
#define ORES_ANALYTICS_CORE_SERVICE_PRICING_MODEL_CONFIG_SERVICE_HPP

#include "ores.analytics.api/domain/pricing_model_config.hpp"
#include "ores.analytics.core/export.hpp"
#include "ores.analytics.core/repository/pricing_model_config_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::service {

/**
 * @brief Service for managing pricing model configurations.
 *
 * Provides a higher-level interface for pricing model configuration operations,
 * wrapping the underlying repository.
 */
class ORES_ANALYTICS_CORE_EXPORT pricing_model_config_service {
private:
    inline static std::string_view logger_name =
        "ores.analytics.service.pricing_model_config_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a pricing_model_config_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit pricing_model_config_service(context ctx);

    /**
     * @brief Lists pricing model configurations with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of pricing model configurations for the requested page.
     */
    std::vector<domain::pricing_model_config> list_configs(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active pricing model configurations.
     *
     * @return Total number of active pricing model configurations.
     */
    std::uint32_t count_configs();


    /**
     * @brief Retrieves a single pricing model configuration as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The pricing model configuration at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_config> get_config_at_version(const std::string& id,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single pricing model configuration by its primary key.
     *
     * @return The pricing model configuration if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_config> get_config(const std::string& id);

    /**
     * @brief Retrieves a single pricing model configuration by its uuid primary key.
     *
     * @return The pricing model configuration if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_config> find_config(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single pricing model configuration by its name.
     *
     * @return The pricing model configuration if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_config> find_config_by_code(const std::string& name);

    /**
     * @brief Saves a pricing model configuration (creates or updates).
     *
     * @param config The pricing model configuration to save.
     * @throws std::exception on failure.
     */
    void save_config(const domain::pricing_model_config& config);

    /**
     * @brief Saves a batch of pricing model configurations.
     *
     * @param configs The pricing model configurations to save.
     * @throws std::exception on failure.
     */
    void save_configs(const std::vector<domain::pricing_model_config>& configs);

    /**
     * @brief Deletes a pricing model configuration by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_config(const std::string& id);

    /**
     * @brief Removes a pricing model configuration by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_config(const boost::uuids::uuid& id);

    /**
     * @brief Deletes pricing model configurations by their primary keys.
     */
    void delete_configs(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a pricing model configuration.
     */
    std::vector<domain::pricing_model_config> get_config_history(const std::string& id);

    /**
     * @brief Retrieves all historical versions of a pricing model configuration
     * by its uuid primary key.
     */
    std::vector<domain::pricing_model_config> get_config_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::pricing_model_config_repository repo_;
};

}

#endif
