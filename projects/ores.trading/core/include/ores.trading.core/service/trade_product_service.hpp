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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_PRODUCT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_PRODUCT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.trading.api/domain/trade_product.hpp"
#include "ores.trading.api/messaging/trade_product_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_product_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trade products.
 *
 * Provides a higher-level interface for trade product operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_product_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_product_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_product_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_product_service(const context& ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    ::ores::trading::messaging::list_trade_products_response
    list_trade_products(const ::ores::trading::messaging::list_trade_products_request& request);
    ::ores::trading::messaging::get_trade_product_response
    get_trade_product(const ::ores::trading::messaging::get_trade_product_request& request);
    ::ores::trading::messaging::get_many_trade_products_response get_many_trade_products(
        const ::ores::trading::messaging::get_many_trade_products_request& request);
    ::ores::trading::messaging::put_trade_product_response
    put_trade_product(const ::ores::trading::messaging::put_trade_product_request& request);
    ::ores::trading::messaging::put_many_trade_products_response put_many_trade_products(
        const ::ores::trading::messaging::put_many_trade_products_request& request);
    ::ores::trading::messaging::delete_trade_product_response
    delete_trade_product(const ::ores::trading::messaging::delete_trade_product_request& request);
    ::ores::trading::messaging::delete_many_trade_products_response delete_many_trade_products(
        const ::ores::trading::messaging::delete_many_trade_products_request& request);
    ::ores::trading::messaging::list_by_trade_id_trade_products_response
    list_by_trade_id_trade_products(
        const ::ores::trading::messaging::list_by_trade_id_trade_products_request& request);
    ::ores::trading::messaging::list_trade_product_versions_response list_trade_product_versions(
        const ::ores::trading::messaging::list_trade_product_versions_request& request);
    ::ores::trading::messaging::get_trade_product_version_response get_trade_product_version(
        const ::ores::trading::messaging::get_trade_product_version_request& request);
    /**@}*/

    /**
     * @brief Lists trade products with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade products for the requested page.
     */
    std::vector<domain::trade_product> list_products(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade products.
     *
     * @return Total number of active trade products.
     */
    std::uint32_t count_products();


    /**
     * @brief Lists trade products filtered by trade_id, with pagination.
     *
     * @param trade_id The trade_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching trade products for the requested page.
     */
    std::vector<domain::trade_product> list_products_by_trade_id(const std::string& trade_id,
                                                                 std::uint32_t offset,
                                                                 std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade products filtered by trade_id.
     *
     * @param trade_id The trade_id to filter by.
     * @return Total number of matching trade products.
     */
    std::uint32_t count_products_by_trade_id(const std::string& trade_id);


    /**
     * @brief Retrieves a single trade product as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade product at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_product> get_product_at_version(const boost::uuids::uuid& trade_id,
                                                                std::uint32_t version);

    /**
     * @brief Retrieves a single trade product by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The trade product if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_product> get_product(const boost::uuids::uuid& trade_id);

    /**
     * @brief Retrieves a batch of trade products by primary key.
     */
    std::vector<domain::trade_product> get_products(const std::vector<std::string>& trade_ids);

    /**
     * @brief Saves a trade product (creates or updates).
     *
     * @param product The trade product to save.
     * @throws std::exception on failure.
     */
    void save_product(const domain::trade_product& product);

    /**
     * @brief Saves a batch of trade products.
     *
     * @param products The trade products to save.
     * @throws std::exception on failure.
     */
    void save_products(const std::vector<domain::trade_product>& products);

    /**
     * @brief Deletes a trade product by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_product(const boost::uuids::uuid& trade_id);

    /**
     * @brief Deletes trade products by their primary keys.
     */
    void delete_products(const std::vector<std::string>& trade_ids);

    /**
     * @brief Retrieves all historical versions of a trade product.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::trade_product> get_product_history(const std::string& trade_id);

private:
    context ctx_;
    repository::trade_product_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * The caller reads the current row, so a batch reads every row it names
     * in one query rather than one query per change.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param current The row the change names as it is now, or empty.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::trading::messaging::trade_product_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::trade_product>& current,
                   domain::trade_product& out);
};

}

#endif
