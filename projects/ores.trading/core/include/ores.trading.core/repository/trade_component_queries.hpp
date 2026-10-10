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
#ifndef ORES_TRADING_CORE_REPOSITORY_TRADE_COMPONENT_QUERIES_HPP
#define ORES_TRADING_CORE_REPOSITORY_TRADE_COMPONENT_QUERIES_HPP

#include "ores.database/domain/context.hpp"
#include "ores.trading.api/domain/trade_additional_field.hpp"
#include "ores.trading.api/domain/trade_identifier.hpp"
#include "ores.trading.api/domain/trade_party_role.hpp"
#include "ores.trading.api/domain/trade_portfolio.hpp"
#include "ores.trading.core/export.hpp"
#include <string>
#include <vector>

namespace ores::trading::repository {

using context = ores::database::context;

/**
 * @brief Reads the child rows of a trade whose key is more than the trade.
 *
 * Four trade components are keyed by the trade and by a second field: an
 * identifier by its scheme, an additional field by its ordinal, a portfolio
 * by its ordinal and a party role by the role. The generated repositories
 * read one full key tuple or one page of the whole table, and neither answers
 * "every row of these trades". The functions here do, in the same shape as
 * the instrument reads beside them, and order each result by the full key so
 * a fold over it is deterministic.
 */
/**@{*/

ORES_TRADING_CORE_EXPORT std::vector<domain::trade_additional_field>
read_additional_fields_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

ORES_TRADING_CORE_EXPORT std::vector<domain::trade_identifier>
read_identifiers_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

ORES_TRADING_CORE_EXPORT std::vector<domain::trade_portfolio>
read_portfolios_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief The digest of every economic component a trade holds.
 *
 * The instrument the trade's type routes to, and the legs, amounts, rates,
 * amortizations, schedules, strikes, options and premiums beneath it. Those
 * are the terms a customer agreed to; a comment, a lifecycle state or an
 * organisation is not, and none of those is read here.
 *
 * The instrument is read by @c instrument_table_for, so a family the
 * catalogue routes nowhere contributes nothing rather than being guessed at.
 */
ORES_TRADING_CORE_EXPORT std::vector<std::string> read_instrument_digests(
    context ctx, const std::string& trade_id, const std::string& trade_type);

ORES_TRADING_CORE_EXPORT std::vector<domain::trade_party_role>
read_party_roles_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**@}*/

}

#endif
