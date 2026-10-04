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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_ENVELOPE_READER_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_ENVELOPE_READER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_envelope_data.hpp"
#include "ores.trading.core/export.hpp"
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Rebuilds the trade envelope container from the tables that hold it.
 *
 * An envelope is one row plus two child lists, and a reader that walked
 * the trade table one trade at a time would pay three queries per trade.
 * This reader takes the trades as a set and issues one query per table.
 *
 * A booked trade's envelope comes from its anchor and components: the
 * counterparty and netting set names the booking recorded, the names of the
 * portfolios it is reported in and its additional fields. A trade with no
 * anchor keeps the envelope its own tables hold, and a trade with neither is
 * absent from the result.
 */
class ORES_TRADING_CORE_EXPORT trade_envelope_reader {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_envelope_reader";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit trade_envelope_reader(context ctx);

    /**
     * @brief Reads the envelopes of a set of trades, keyed by trade id.
     *
     * @param trade_ids UUIDs of the trades to read, as text.
     * @return One container per trade that has an envelope row.
     */
    std::unordered_map<std::string, domain::trade_envelope_data>
    read_envelopes(const std::vector<std::string>& trade_ids) const;

private:
    void read_booked_envelopes(
        const std::vector<std::string>& trade_ids,
        std::unordered_map<std::string, domain::trade_envelope_data>& envelopes) const;

    context ctx_;
};

}

#endif
