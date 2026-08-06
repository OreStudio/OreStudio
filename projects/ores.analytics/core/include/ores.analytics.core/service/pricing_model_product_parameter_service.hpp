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
#ifndef ORES_ANALYTICS_CORE_SERVICE_PRICING_MODEL_PRODUCT_PARAMETER_SERVICE_HPP
#define ORES_ANALYTICS_CORE_SERVICE_PRICING_MODEL_PRODUCT_PARAMETER_SERVICE_HPP

#include "ores.analytics.api/domain/pricing_model_product_parameter.hpp"
#include "ores.analytics.core/export.hpp"
#include "ores.analytics.core/repository/pricing_model_product_parameter_repository.hpp"
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
 * @brief Service for managing pricing model product parameters.
 *
 * Provides a higher-level interface for pricing model product parameter operations,
 * wrapping the underlying repository.
 */
class ORES_ANALYTICS_CORE_EXPORT pricing_model_product_parameter_service {
private:
    inline static std::string_view logger_name =
        "ores.analytics.service.pricing_model_product_parameter_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a pricing_model_product_parameter_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit pricing_model_product_parameter_service(context ctx);

    /**
     * @brief Lists pricing model product parameters with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of pricing model product parameters for the requested page.
     */
    std::vector<domain::pricing_model_product_parameter> list_parameters(std::uint32_t offset,
                                                                         std::uint32_t limit);

    /**
     * @brief Gets the total count of active pricing model product parameters.
     *
     * @return Total number of active pricing model product parameters.
     */
    std::uint32_t count_parameters();


    /**
     * @brief Retrieves a single pricing model product parameter as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The pricing model product parameter at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_product_parameter>
    get_parameter_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single pricing model product parameter by its primary key.
     *
     * @return The pricing model product parameter if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_product_parameter> get_parameter(const std::string& id);

    /**
     * @brief Retrieves a single pricing model product parameter by its uuid primary key.
     *
     * @return The pricing model product parameter if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_product_parameter>
    find_parameter(const boost::uuids::uuid& id);

    /**
     * @brief Saves a pricing model product parameter (creates or updates).
     *
     * @param parameter The pricing model product parameter to save.
     * @throws std::exception on failure.
     */
    void save_parameter(const domain::pricing_model_product_parameter& parameter);

    /**
     * @brief Saves a batch of pricing model product parameters.
     *
     * @param parameters The pricing model product parameters to save.
     * @throws std::exception on failure.
     */
    void save_parameters(const std::vector<domain::pricing_model_product_parameter>& parameters);

    /**
     * @brief Deletes a pricing model product parameter by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_parameter(const std::string& id);

    /**
     * @brief Removes a pricing model product parameter by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_parameter(const boost::uuids::uuid& id);

    /**
     * @brief Deletes pricing model product parameters by their primary keys.
     */
    void delete_parameters(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a pricing model product parameter.
     */
    std::vector<domain::pricing_model_product_parameter>
    get_parameter_history(const std::string& id);

    /**
     * @brief Retrieves all historical versions of a pricing model product parameter
     * by its uuid primary key.
     */
    std::vector<domain::pricing_model_product_parameter>
    get_parameter_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::pricing_model_product_parameter_repository repo_;
};

}

#endif
