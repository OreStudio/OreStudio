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
#ifndef ORES_TRADING_API_DOMAIN_BOND_INSTRUMENT_ROWS_HPP
#define ORES_TRADING_API_DOMAIN_BOND_INSTRUMENT_ROWS_HPP

#include "ores.trading.api/domain/ascot.hpp"
#include "ores.trading.api/domain/bond_forward.hpp"
#include "ores.trading.api/domain/bond_future.hpp"
#include "ores.trading.api/domain/bond_instrument_data.hpp"
#include "ores.trading.api/domain/bond_issue_leg.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amortization.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amount.hpp"
#include "ores.trading.api/domain/bond_issue_leg_rate.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule_date.hpp"
#include "ores.trading.api/domain/bond_leg.hpp"
#include "ores.trading.api/domain/bond_leg_amortization.hpp"
#include "ores.trading.api/domain/bond_leg_amount.hpp"
#include "ores.trading.api/domain/bond_leg_rate.hpp"
#include "ores.trading.api/domain/bond_option.hpp"
#include "ores.trading.api/domain/bond_repo.hpp"
#include "ores.trading.api/domain/bond_trs.hpp"
#include "ores.trading.api/domain/instrument_option.hpp"
#include "ores.trading.api/domain/instrument_option_exercise_fee.hpp"
#include "ores.trading.api/domain/instrument_option_exercise_price.hpp"
#include "ores.trading.api/domain/instrument_option_payment_date.hpp"
#include "ores.trading.api/domain/instrument_option_premium.hpp"
#include "ores.trading.api/domain/instrument_schedule.hpp"
#include "ores.trading.api/domain/instrument_schedule_date.hpp"
#include "ores.trading.api/domain/instrument_strike.hpp"
#include "ores.trading.api/domain/instrument_underlying.hpp"
#include "ores.trading.api/export.hpp"
#include <optional>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief The rows a bond security owns, as the issue-keyed tables hold them.
 *
 * A bond's own legs belong to the security rather than to the trade, so they
 * and everything they state key on the issue.
 */
struct issue_leg_rows final {
    std::vector<bond_issue_leg> legs;
    std::vector<bond_issue_leg_amount> amounts;
    std::vector<bond_issue_leg_rate> rates;
    std::vector<bond_issue_leg_amortization> amortizations;
    std::vector<bond_issue_leg_schedule> schedules;
    std::vector<bond_issue_leg_schedule_date> schedule_dates;
};

/**
 * @brief The rows a bond trade owns, as the trade-keyed tables hold them.
 *
 * The rows stay in the order their query returned: by instrument, then by the
 * owner's role and number, then by the child's own ordinal. The product's fact
 * row is the one its trade type names; the others stay empty.
 */
struct bond_trade_rows final {
    std::vector<bond_leg> legs;
    std::vector<bond_leg_amount> amounts;
    std::vector<bond_leg_rate> rates;
    std::vector<bond_leg_amortization> amortizations;
    std::vector<instrument_schedule> schedules;
    std::vector<instrument_schedule_date> schedule_dates;
    std::optional<instrument_option> option;
    std::vector<instrument_option_premium> option_premiums;
    std::vector<instrument_option_exercise_fee> option_exercise_fees;
    std::vector<instrument_option_payment_date> option_payment_dates;
    std::vector<instrument_option_exercise_price> option_exercise_prices;
    std::optional<instrument_strike> strike;
    std::optional<bond_forward> forward;
    std::optional<bond_option> bond_option;
    std::optional<bond_trs> trs;
    std::optional<bond_repo> repo;
    std::optional<bond_future> future;
    std::optional<ascot> ascot;
    std::vector<instrument_underlying> underlyings;
};

/**
 * @brief Completes a bond's container from its trade's rows and its security's.
 *
 * The export reader reads the rows and calls this; a caller that already holds
 * the rows, such as a test, calls it the same way.
 */
ORES_TRADING_API_EXPORT void assemble_bond(bond_instrument_data& data,
                                           const bond_trade_rows& trade,
                                           const issue_leg_rows& issue);

/// The security's legs as the export writes them, in the order the issue lists them.
[[nodiscard]] ORES_TRADING_API_EXPORT std::vector<bond_leg_data>
bond_legs_of(const issue_leg_rows& rows);

}

#endif
