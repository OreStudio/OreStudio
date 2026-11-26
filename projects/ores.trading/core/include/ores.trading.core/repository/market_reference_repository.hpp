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
#ifndef ORES_TRADING_CORE_REPOSITORY_MARKET_REFERENCE_REPOSITORY_HPP
#define ORES_TRADING_CORE_REPOSITORY_MARKET_REFERENCE_REPOSITORY_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.logging/logger.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::repository {

/**
 * @brief The market data addresses a set of trades needs.
 *
 * A trade's instrument rows name the indices, curves and underlyings it floats
 * on, and each of those is an oresmd address. The list is derived from the
 * trades rather than stated by a caller, so a run gathers the market data its
 * own population needs and no more.
 */
class ORES_TRADING_CORE_EXPORT market_reference_repository {
private:
    inline static std::string_view logger_name =
        "ores.trading.repository.market_reference_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief The distinct addresses the trades need, in address order.
     *
     * A trade whose rows hold no address contributes nothing rather than failing the read: an
     * imported trade is in that state until its names are resolved, and a run says what it
     * needs from the trades that do name something.
     *
     * @param as_of The transaction time the trades are read at, or nothing for the current
     *              rows. Read as text, so a date and a timestamp are both admitted.
     */
    [[nodiscard]] std::vector<std::string> read(const context& ctx,
                                                const std::vector<std::string>& trade_ids,
                                                const std::optional<std::string>& as_of);

    /**
     * @brief What each trade needs, one entry per trade named, in the order named.
     *
     * A trade the request names twice answers once. A trade whose rows hold no address keeps
     * its entry with no addresses, which is what tells a trade that names nothing from one the
     * request did not name.
     *
     * @param as_of The transaction time the trades are read at, or nothing for the current
     *              rows, as =read= admits it.
     */
    [[nodiscard]] std::vector<messaging::trade_requirement>
    read_by_trade(const context& ctx,
                  const std::vector<std::string>& trade_ids,
                  const std::optional<std::string>& as_of);
};

/**
 * @brief What a row of one trade's addresses says: the trade, and one address it needs.
 */
struct market_reference_row final {
    std::string trade_id;
    std::string oresmd_uri;
};

/**
 * @brief Folds the resolution's rows into one answer per trade the caller named.
 *
 * The answer states an entry for every trade asked about, in the order asked, and a trade
 * that repeats answers once, on its first place. This is a plain function over rows so the
 * reading is a thing a test can state without a database: a trade that names nothing and a
 * trade that was never asked about are one absence apart, and a caller that cannot tell them
 * apart cannot say which trades to investigate.
 */
ORES_TRADING_CORE_EXPORT std::vector<messaging::trade_requirement>
trade_requirements_from(const std::vector<market_reference_row>& rows,
                        const std::vector<std::string>& trade_ids);

}

#endif
