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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trades.
 *
 * Provides a higher-level interface for trade operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_service(context ctx);

    /**
     * @brief Lists trades with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trades for the requested page.
     */
    std::vector<domain::trade> list_trades(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active trades.
     *
     * @return Total number of active trades.
     */
    std::uint32_t count_trades();

    /**
     * @brief Lists a page of trades under one node_id.
     *
     * An empty node_id covers every trade the
     * tenant can see, so the caller need not special-case the unfiltered
     * listing.
     */
    std::vector<domain::trade>
    list_trades(std::uint32_t offset, std::uint32_t limit, const std::string& node_id);

    /**
     * @brief Counts the trades under one node_id,
     * for the pager the page itself cannot supply.
     */
    std::uint32_t count_trades(const std::string& node_id);


    /**
     * @brief Retrieves a single trade as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::trade> get_trade_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single trade by its primary key.
     *
     * @return The trade if found, std::nullopt otherwise.
     */
    std::optional<domain::trade> get_trade(const std::string& id);

    /**
     * @brief Saves a trade (creates or updates).
     *
     * @param trade The trade to save.
     * @throws std::exception on failure.
     */
    void save_trade(const domain::trade& trade);

    /**
     * @brief Saves a batch of trades.
     *
     * @param trades The trades to save.
     * @throws std::exception on failure.
     */
    void save_trades(const std::vector<domain::trade>& trades);

    /**
     * @brief Deletes a trade by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_trade(const std::string& id);

    /**
     * @brief Deletes trades by their primary keys.
     */
    void delete_trades(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a trade.
     */
    std::vector<domain::trade> get_trade_history(const std::string& id);

private:
    context ctx_;
    repository::trade_repository repo_;
};

}

#endif
