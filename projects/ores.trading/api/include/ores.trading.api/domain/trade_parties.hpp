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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_PARTIES_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_PARTIES_HPP

#include <boost/uuid/uuid.hpp>
#include <optional>

namespace ores::trading::domain {

/**
 * @brief Counterparty and book/portfolio relationships for a trade.
 *
 * One of five field-groups that compose trading::domain::trade via
 * rfl::Flatten<T>.  See trade_identity_field_group.json for the
 * rationale behind the sub-struct decomposition.
 *
 * This group carries the party relationship fields (book, portfolio,
 * counterparty, and optional successor trade).
 */
struct trade_parties {
    /**
     * @brief Book that owns this trade (soft FK to ores_refdata_books_tbl).
     */
    boost::uuids::uuid book_id;

    /**
     * @brief Portfolio this trade belongs to (soft FK to ores_refdata_portfolios_tbl).
     */
    boost::uuids::uuid portfolio_id;

    /**
     * @brief Optional counterparty (soft FK to ores_refdata_counterparties_tbl).
     */
    std::optional<boost::uuids::uuid> counterparty_id;

    /**
     * @brief UUID of the trade that replaced this one (e.g. after novation). Absent for active
     * trades.
     */
    std::optional<boost::uuids::uuid> successor_trade_id;
};

}

#endif
