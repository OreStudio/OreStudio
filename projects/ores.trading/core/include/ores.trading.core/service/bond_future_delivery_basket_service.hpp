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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_FUTURE_DELIVERY_BASKET_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_FUTURE_DELIVERY_BASKET_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_future_delivery_basket.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_future_delivery_basket_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond future delivery basket identifiers.
 *
 * Provides a higher-level interface for bond future delivery basket identifier operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_future_delivery_basket_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.bond_future_delivery_basket_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_future_delivery_basket_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_future_delivery_basket_service(context ctx);

    /**
     * @brief Lists bond future delivery basket identifiers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond future delivery basket identifiers for the requested page.
     */
    std::vector<domain::bond_future_delivery_basket> list_delivery_basket_ids(std::uint32_t offset,
                                                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond future delivery basket identifiers.
     *
     * @return Total number of active bond future delivery basket identifiers.
     */
    std::uint32_t count_delivery_basket_ids();


    /**
     * @brief Retrieves a single bond future delivery basket identifier as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond future delivery basket identifier at that version if found, std::nullopt
     * otherwise.
     */
    std::optional<domain::bond_future_delivery_basket>
    get_delivery_basket_id_at_version(const std::string& instrument_id,
                                      const std::string& sequence_number,
                                      std::uint32_t version);

    /**
     * @brief Retrieves a single bond future delivery basket identifier by its primary key.
     *
     * @return The bond future delivery basket identifier if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_future_delivery_basket>
    get_delivery_basket_id(const std::string& instrument_id, const std::string& sequence_number);

    /**
     * @brief Saves a bond future delivery basket identifier (creates or updates).
     *
     * @param delivery_basket_id The bond future delivery basket identifier to save.
     * @throws std::exception on failure.
     */
    void save_delivery_basket_id(const domain::bond_future_delivery_basket& delivery_basket_id);

    /**
     * @brief Saves a batch of bond future delivery basket identifiers.
     *
     * @param delivery_basket_ids The bond future delivery basket identifiers to save.
     * @throws std::exception on failure.
     */
    void save_delivery_basket_ids(
        const std::vector<domain::bond_future_delivery_basket>& delivery_basket_ids);

    /**
     * @brief Deletes a bond future delivery basket identifier by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_delivery_basket_id(const std::string& instrument_id,
                                   const std::string& sequence_number);

    /**
     * @brief Deletes bond future delivery basket identifiers by their primary keys.
     */
    void delete_delivery_basket_ids(const std::vector<std::string>& instrument_ids,
                                    const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a bond future delivery basket identifier.
     */
    std::vector<domain::bond_future_delivery_basket>
    get_delivery_basket_id_history(const std::string& instrument_id,
                                   const std::string& sequence_number);

private:
    context ctx_;
    repository::bond_future_delivery_basket_repository repo_;
};

}

#endif
