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
#ifndef ORES_TRADING_SERVICE_TRADE_SERVICE_HPP
#define ORES_TRADING_SERVICE_TRADE_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.trading/domain/trade.hpp"
#include "ores.trading/repository/trade_repository.hpp"

namespace ores::trading::service {

/**
 * @brief Service for managing trades.
 */
class trade_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.trade_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit trade_service(context ctx);

    std::vector<domain::trade> list_trades();

    std::vector<domain::trade>
    list_trades(std::uint32_t offset, std::uint32_t limit);

    std::vector<domain::trade>
    list_trades_filtered(std::uint32_t offset, std::uint32_t limit,
        std::optional<boost::uuids::uuid> book_id,
        std::optional<boost::uuids::uuid> portfolio_id);

    std::uint32_t count_trades();

    std::uint32_t count_trades_filtered(
        std::optional<boost::uuids::uuid> book_id,
        std::optional<boost::uuids::uuid> portfolio_id);

    std::optional<domain::trade>
    find_trade(const std::string& id);

    void save_trade(const domain::trade& v);

    void remove_trade(const std::string& id);

    std::vector<domain::trade>
    get_trade_history(const std::string& id);

private:
    context ctx_;
    repository::trade_repository repo_;
};

}

#endif
