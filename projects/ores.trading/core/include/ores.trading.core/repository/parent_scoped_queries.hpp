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
#include "ores.trading.core/export.hpp"
#include "ores.trading.api/domain/trade_envelope.hpp"
#include "ores.trading.api/domain/trade_envelope_additional_field.hpp"
#include "ores.trading.api/domain/trade_envelope_portfolio_id.hpp"
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
 * @brief Reads the envelopes of a set of trades.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose envelopes to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::trade_envelope>
read_envelopes_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief Reads the portfolio identifiers of a set of trade envelopes.
 *
 * Rows come back in envelope order, then in ordinal order, so a caller
 * that appends as it walks rebuilds the document's list order.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose envelopes to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::trade_envelope_portfolio_id>
read_portfolio_ids_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**
 * @brief Reads the additional fields of a set of trade envelopes.
 *
 * @param ctx The database context, which carries the tenant.
 * @param trade_ids UUIDs of the trades whose envelopes to read.
 */
ORES_TRADING_CORE_EXPORT std::vector<domain::trade_envelope_additional_field>
read_additional_fields_by_trade_ids(context ctx, const std::vector<std::string>& trade_ids);

/**@}*/

}

#endif
