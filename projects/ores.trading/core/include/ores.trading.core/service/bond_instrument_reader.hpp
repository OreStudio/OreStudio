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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_INSTRUMENT_READER_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_INSTRUMENT_READER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_instrument_data.hpp"
#include "ores.trading.api/domain/bond_issue_leg.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amortization.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amount.hpp"
#include "ores.trading.api/domain/bond_issue_leg_rate.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule_date.hpp"
#include "ores.trading.core/export.hpp"
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::trading::service {

/**
 * @brief The rows a bond security owns, as the issue-keyed tables hold them.
 *
 * A bond's own legs belong to the security rather than to the trade, so they
 * and everything they state key on the issue.
 */
struct issue_leg_rows final {
    std::vector<domain::bond_issue_leg> legs;
    std::vector<domain::bond_issue_leg_amount> amounts;
    std::vector<domain::bond_issue_leg_rate> rates;
    std::vector<domain::bond_issue_leg_amortization> amortizations;
    std::vector<domain::bond_issue_leg_schedule> schedules;
    std::vector<domain::bond_issue_leg_schedule_date> schedule_dates;
};

/// The security's legs as the export writes them, in the order the issue lists them.
[[nodiscard]] ORES_TRADING_CORE_EXPORT std::vector<domain::bond_leg_data>
bond_legs_of(const issue_leg_rows& rows);

/**
 * @brief Rebuilds bond instrument containers from the rows that hold them.
 *
 * A bond instrument is spread over one header row, one issue row, the
 * issue's two keyed child lists, the leg family and one fact row chosen
 * by the instrument's type code. The export path holds the trade
 * identifiers and needs the whole container for each, which no single
 * generated repository read answers.
 *
 * One issue serves every instrument of one security, so it is fetched
 * once per issue rather than once per instrument, and each child list
 * costs one query for the whole batch.
 */
class ORES_TRADING_CORE_EXPORT bond_instrument_reader {
private:
    inline static std::string_view logger_name = "ores.trading.service.bond_instrument_reader";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit bond_instrument_reader(context ctx);

    /**
     * @brief Reads the containers of a set of bond instruments.
     *
     * @param trade_ids UUIDs of the trades whose bond instruments to read.
     * @return The containers keyed by instrument id as text. An
     * instrument with no header row is absent from the result.
     */
    std::unordered_map<std::string, domain::bond_instrument_data>
    read_instruments(const std::vector<std::string>& trade_ids) const;

private:
    context ctx_;
};

}

#endif
