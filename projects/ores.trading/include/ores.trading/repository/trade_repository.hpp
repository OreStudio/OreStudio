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
#ifndef ORES_TRADING_REPOSITORY_TRADE_REPOSITORY_HPP
#define ORES_TRADING_REPOSITORY_TRADE_REPOSITORY_HPP

#include <string>
#include <vector>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.trading/domain/trade.hpp"

namespace ores::trading::repository {

/**
 * @brief Reads and writes trades to data storage.
 */
class trade_repository {
private:
    inline static std::string_view logger_name =
        "ores.trading.repository.trade_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    std::string sql();

    void write(context ctx, const domain::trade& v);
    void write(context ctx, const std::vector<domain::trade>& v);

    std::vector<domain::trade> read_latest(context ctx);
    std::vector<domain::trade>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);
    std::vector<domain::trade>
    read_latest(context ctx, const std::string& id);
    std::vector<domain::trade>
    read_all(context ctx, const std::string& id);

    std::uint32_t count_latest(context ctx);

    std::vector<domain::trade> read_latest_filtered(
        context ctx,
        std::uint32_t offset, std::uint32_t limit,
        std::optional<boost::uuids::uuid> book_id,
        std::optional<boost::uuids::uuid> portfolio_id);

    std::uint32_t count_latest_filtered(
        context ctx,
        std::optional<boost::uuids::uuid> book_id,
        std::optional<boost::uuids::uuid> portfolio_id);

    void remove(context ctx, const std::string& id);
};

}

#endif
