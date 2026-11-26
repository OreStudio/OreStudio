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
#include "ores.trading.core/export.hpp"
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::repository {

/// What one trade needs: the trade, and the addresses its own rows name.
struct trade_requirement final {
    std::string trade_id;
    std::vector<std::string> oresmd_uris;
};

/// What a row of the resolution says: the trade, and one address it needs.
struct market_reference_row final {
    std::string trade_id;
    std::string oresmd_uri;
};

/**
 * @brief The market data addresses each trade needs, and the set of them.
 *
 * A trade's instrument rows name the indices, curves and underlyings it floats
 * on, and each of those is an oresmd address. The addresses are derived from the
 * trades rather than stated by a caller, so a run gathers the market data its own
 * population needs and no more.
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
     * @brief What each trade needs, one entry per trade named, in the order named.
     *
     * A trade the request names twice answers once. A trade whose rows hold no address keeps
     * its entry with no addresses, which is what tells a trade that names nothing from one the
     * request did not name.
     *
     * @param as_of The transaction time the trades are read at, or nothing for the current
     *              rows. Read as text, so a date and a timestamp are both admitted, and a bare
     *              date is read at the close of its day.
     */
    [[nodiscard]] std::vector<trade_requirement>
    read_by_trade(const context& ctx,
                  const std::vector<std::string>& trade_ids,
                  const std::optional<std::string>& as_of);
};

/**
 * @brief Folds the resolution's rows into one answer per trade the caller named.
 *
 * The answer states an entry for every trade asked about, in the order asked, and a trade
 * that repeats answers once, on its first place. The trade identifiers are matched by value
 * and not by their text, so a caller that states a trade in another spelling of the same
 * identifier — upper case, braced, or with a =urn:uuid:= prefix — still finds its addresses.
 *
 * Two absences are not the same, and only one of them is this function's to state. A trade the
 * request does not name is absent from the answer. A trade the request names that the caller
 * cannot read — no such trade, or one row-level security hides — answers present and empty,
 * exactly as a trade whose instrument rows name nothing does: the store tells a caller who may
 * not see a row nothing about why it sees nothing, and a caller that must tell the two apart
 * reads the trade set first.
 */
ORES_TRADING_CORE_EXPORT std::vector<trade_requirement>
trade_requirements_from(const std::vector<market_reference_row>& rows,
                        const std::vector<std::string>& trade_ids);

}

#endif
