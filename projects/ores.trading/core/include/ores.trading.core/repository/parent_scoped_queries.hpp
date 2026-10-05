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
#ifndef ORES_TRADING_CORE_REPOSITORY_PARENT_SCOPED_QUERIES_HPP
#define ORES_TRADING_CORE_REPOSITORY_PARENT_SCOPED_QUERIES_HPP

#include "ores.database/domain/context.hpp"
#include "ores.trading.api/domain/bond_forward.hpp"
#include "ores.trading.api/domain/bond_issue_call_date.hpp"
#include "ores.trading.api/domain/bond_issue_conversion_target.hpp"
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
#include "ores.trading.api/domain/instrument_option.hpp"
#include "ores.trading.api/domain/instrument_option_exercise_fee.hpp"
#include "ores.trading.api/domain/instrument_option_payment_date.hpp"
#include "ores.trading.api/domain/instrument_option_premium.hpp"
#include "ores.trading.api/domain/instrument_schedule.hpp"
#include "ores.trading.api/domain/instrument_schedule_date.hpp"
#include "ores.trading.api/domain/instrument_strike.hpp"
#include "ores.trading.core/export.hpp"
#include <string>
#include <vector>

namespace ores::trading::repository {

using context = ores::database::context;

/**
 * @brief Reads the child rows of a set of parents.
 *
 * A child table is keyed by its parent and by the child's own ordinal.
 * The generated repositories read one key tuple, or one page of the whole
 * table, and neither answers "every child row of these parents". The
 * functions here do, with one query per table rather than one per parent.
 *
 * The entity and mapper types are the generated ones, so a column change
 * reaches these queries through regeneration. The queries themselves are
 * hand written, because the entity templates emit no reading of this
 * shape.
 */
/**@{*/

/**
 * @brief Reads the call dates of a set of bond issues.
 *
 * Rows come back in issue order, then in ordinal order.
 *
 * @param ctx The database context, which carries the tenant.
 * @param issue_ids UUIDs of the issues whose call dates to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::bond_issue_call_date>
read_call_dates_by_issue_ids(context ctx, const std::vector<std::string>& issue_ids);

/**
 * @brief Reads the conversion targets of a set of bond issues.
 *
 * Rows come back in issue order, then in ordinal order.
 *
 * @param ctx The database context, which carries the tenant.
 * @param issue_ids UUIDs of the issues whose conversion targets to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::bond_issue_conversion_target>
read_conversion_targets_by_issue_ids(context ctx, const std::vector<std::string>& issue_ids);

/**
 * @brief Reads the legs of a set of instruments.
 *
 * Rows come back in instrument order, then in role and ordinal order.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose legs to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::bond_leg>
read_legs_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief Reads the named amounts of a set of instruments' legs.
 *
 * Rows come back in instrument order, then in role and ordinal order.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose leg amounts to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::bond_leg_amount>
read_leg_amounts_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief Reads the rate group rows of a set of instruments' legs.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose leg rates to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::bond_leg_rate>
read_leg_rates_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief Reads the amortizations of a set of instruments' legs.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose amortizations to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::bond_leg_amortization>
read_leg_amortizations_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief Reads the legs of a set of bond issues.
 *
 * A bond's own legs belong to the security, so they key on the issue
 * rather than on the trade. The three product legs keep the trade-keyed
 * read above.
 *
 * @param ctx The database context, which carries the tenant.
 * @param issue_ids UUIDs of the issues whose legs to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::bond_issue_leg>
read_issue_legs_by_issue_ids(context ctx, const std::vector<std::string>& issue_ids);

/**
 * @brief Reads the named amounts of a set of bond issues' legs.
 *
 * @param ctx The database context, which carries the tenant.
 * @param issue_ids UUIDs of the issues whose leg amounts to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::bond_issue_leg_amount>
read_issue_leg_amounts_by_issue_ids(context ctx, const std::vector<std::string>& issue_ids);

/**
 * @brief Reads the rate group rows of a set of bond issues' legs.
 *
 * @param ctx The database context, which carries the tenant.
 * @param issue_ids UUIDs of the issues whose leg rates to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::bond_issue_leg_rate>
read_issue_leg_rates_by_issue_ids(context ctx, const std::vector<std::string>& issue_ids);

/**
 * @brief Reads the amortizations of a set of bond issues' legs.
 *
 * @param ctx The database context, which carries the tenant.
 * @param issue_ids UUIDs of the issues whose leg amortizations to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::bond_issue_leg_amortization>
read_issue_leg_amortizations_by_issue_ids(context ctx, const std::vector<std::string>& issue_ids);

/**
 * @brief Reads the schedules of a set of bond issues' legs.
 *
 * @param ctx The database context, which carries the tenant.
 * @param issue_ids UUIDs of the issues whose leg schedules to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::bond_issue_leg_schedule>
read_issue_schedules_by_issue_ids(context ctx, const std::vector<std::string>& issue_ids);

/**
 * @brief Reads the dates of a set of bond issues' leg schedules.
 *
 * @param ctx The database context, which carries the tenant.
 * @param issue_ids UUIDs of the issues whose leg schedule dates to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::bond_issue_leg_schedule_date>
read_issue_schedule_dates_by_issue_ids(context ctx, const std::vector<std::string>& issue_ids);

/**
 * @brief Reads the schedules of a set of instruments, whichever owner stated them.
 *
 * A row's owner is a leg list, an option block or a return block, so the
 * read covers the leg family and the two owners beside it.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose schedules to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::instrument_schedule>
read_schedules_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief Reads the dates of a set of instruments' schedules.
 *
 * Rows come back in instrument order, then in owner, role and schedule
 * order, then in the date list's own order.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose schedule dates to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::instrument_schedule_date>
read_schedule_dates_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief Reads the option row of a set of instruments.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose option row to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::instrument_option>
read_options_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief Reads the premiums of a set of instruments' option rows.
 *
 * Rows come back in instrument order, then in ordinal order.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose premiums to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::instrument_option_premium>
read_option_premiums_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief Reads the exercise fees of a set of instruments' option rows.
 *
 * Rows come back in instrument order, then in ordinal order.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose exercise fees to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::instrument_option_exercise_fee>
read_option_exercise_fees_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief Reads the payment dates of a set of instruments' option rows.
 *
 * Rows come back in instrument order, then in ordinal order.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose payment dates to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::instrument_option_payment_date>
read_option_payment_dates_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief Reads the strike of a set of instruments.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose strike to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::instrument_strike>
read_strikes_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief Reads the forward terms of a set of instruments.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose forward terms to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::bond_forward>
read_forwards_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**@}*/

}

#endif
