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
#ifndef ORES_ANALYTICS_CORE_SERVICE_PRICING_MODEL_PRODUCT_SERVICE_HPP
#define ORES_ANALYTICS_CORE_SERVICE_PRICING_MODEL_PRODUCT_SERVICE_HPP

#include "ores.analytics.api/domain/pricing_model_product.hpp"
#include "ores.analytics.core/export.hpp"
#include "ores.analytics.core/repository/pricing_model_product_repository.hpp"
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
 * @brief Service for managing pricing model products.
 *
 * Provides a higher-level interface for pricing model product operations,
 * wrapping the underlying repository.
 */
class ORES_ANALYTICS_CORE_EXPORT pricing_model_product_service {
private:
    inline static std::string_view logger_name =
        "ores.analytics.service.pricing_model_product_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a pricing_model_product_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit pricing_model_product_service(context ctx);

    /**
     * @brief Lists pricing model products with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of pricing model products for the requested page.
     */
    std::vector<domain::pricing_model_product> list_products(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active pricing model products.
     *
     * @return Total number of active pricing model products.
     */
    std::uint32_t count_products();


    /**
     * @brief Retrieves a single pricing model product as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The pricing model product at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_product> get_product_at_version(const std::string& id,
                                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single pricing model product by its primary key.
     *
     * @return The pricing model product if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_product> get_product(const std::string& id);

    /**
     * @brief Retrieves a single pricing model product by its uuid primary key.
     *
     * @return The pricing model product if found, std::nullopt otherwise.
     */
    std::optional<domain::pricing_model_product> find_product(const boost::uuids::uuid& id);

    /**
     * @brief Saves a pricing model product (creates or updates).
     *
     * @param product The pricing model product to save.
     * @throws std::exception on failure.
     */
    void save_product(const domain::pricing_model_product& product);

    /**
     * @brief Saves a batch of pricing model products.
     *
     * @param products The pricing model products to save.
     * @throws std::exception on failure.
     */
    void save_products(const std::vector<domain::pricing_model_product>& products);

    /**
     * @brief Deletes a pricing model product by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_product(const std::string& id);

    /**
     * @brief Removes a pricing model product by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_product(const boost::uuids::uuid& id);

    /**
     * @brief Deletes pricing model products by their primary keys.
     */
    void delete_products(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a pricing model product.
     */
    std::vector<domain::pricing_model_product> get_product_history(const std::string& id);

    /**
     * @brief Retrieves all historical versions of a pricing model product
     * by its uuid primary key.
     */
    std::vector<domain::pricing_model_product> get_product_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::pricing_model_product_repository repo_;
};

}

#endif
