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
#ifndef ORES_TRADING_API_DOMAIN_BOND_PRODUCT_BATCH_HPP
#define ORES_TRADING_API_DOMAIN_BOND_PRODUCT_BATCH_HPP

#include "ores.trading.api/domain/bond_instruments.hpp"
#include "ores.trading.api/domain/instrument_batch.hpp"

namespace ores::trading::domain {

/**
 * @brief Adds a whole bond product to the instrument batch.
 *
 * A product is its header plus the one row set that is its own, and a caller
 * that moved each set by hand would be stating that shape a second time. The
 * append states it once, so a new product states its rows in one place.
 */
inline void append(instrument_batch& batch, bond_product&& product) {
    batch.bond_instruments.push_back(std::move(product.row));
    for (auto& v : product.options)
        batch.bond_options.push_back(std::move(v));
    for (auto& v : product.forwards)
        batch.bond_forwards.push_back(std::move(v));
    for (auto& v : product.futures)
        batch.bond_futures.push_back(std::move(v));
    for (auto& v : product.repos)
        batch.bond_repos.push_back(std::move(v));
    for (auto& v : product.trs)
        batch.bond_trs.push_back(std::move(v));
    for (auto& v : product.ascots)
        batch.ascots.push_back(std::move(v));
    for (auto& v : product.call_dates)
        batch.bond_issue_call_dates.push_back(std::move(v));
    for (auto& v : product.conversion_targets)
        batch.bond_issue_conversion_targets.push_back(std::move(v));
}

}

#endif
