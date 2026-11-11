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
#ifndef ORES_TRADING_API_DOMAIN_BOND_PRODUCT_HPP
#define ORES_TRADING_API_DOMAIN_BOND_PRODUCT_HPP

#include "ores.trading.api/domain/ascot.hpp"
#include "ores.trading.api/domain/bond_forward.hpp"
#include "ores.trading.api/domain/bond_future.hpp"
#include "ores.trading.api/domain/bond_instrument_data.hpp"
#include "ores.trading.api/domain/bond_issue.hpp"
#include "ores.trading.api/domain/bond_issue_call_date.hpp"
#include "ores.trading.api/domain/bond_issue_conversion_target.hpp"
#include "ores.trading.api/domain/bond_issue_leg.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amount.hpp"
#include "ores.trading.api/domain/bond_issue_leg_rate.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule.hpp"
#include "ores.trading.api/domain/bond_leg.hpp"
#include "ores.trading.api/domain/bond_leg_amount.hpp"
#include "ores.trading.api/domain/bond_leg_rate.hpp"
#include "ores.trading.api/domain/bond_option.hpp"
#include "ores.trading.api/domain/bond_repo.hpp"
#include "ores.trading.api/domain/bond_trs.hpp"
#include "ores.trading.api/domain/instrument_option.hpp"
#include "ores.trading.api/domain/instrument_schedule.hpp"
#include "ores.trading.api/domain/instrument_schedule_date.hpp"
#include "ores.trading.api/domain/instrument_underlying.hpp"
#include <vector>

namespace ores::trading::domain {

/**
 * @brief A security and the rows that state it: the issue and its coupon legs.
 *
 * A bond trade holds a security rather than stating one, so the security is
 * written before the trade and keyed by its issue.
 */
struct bond_security final {
    bond_issue issue;
    std::vector<bond_issue_leg> legs;
    std::vector<bond_issue_leg_amount> amounts;
    std::vector<bond_issue_leg_rate> rates;
    std::vector<bond_issue_leg_schedule> schedules;
};

/**
 * @brief A bond trade and the rows that state it.
 *
 * The header holds the security; the product's own fact row is the one its
 * trade type names. A trade's own legs, such as a repo's cash leg, and its
 * option block, schedules and underlyings are keyed by the trade.
 */
struct bond_product final {
    bond_instrument_data row;
    std::vector<bond_option> options;
    std::vector<bond_forward> forwards;
    std::vector<bond_future> futures;
    std::vector<bond_repo> repos;
    std::vector<bond_trs> trs;
    std::vector<ascot> ascots;
    std::vector<bond_issue_call_date> call_dates;
    std::vector<bond_issue_conversion_target> conversion_targets;
    std::vector<bond_leg> legs;
    std::vector<bond_leg_amount> leg_amounts;
    std::vector<bond_leg_rate> leg_rates;
    std::vector<instrument_option> option_block;
    std::vector<instrument_schedule> schedules;
    std::vector<instrument_schedule_date> schedule_dates;
    std::vector<instrument_underlying> underlyings;
};

}

#endif
