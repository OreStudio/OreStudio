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
#ifndef ORES_TRADING_DOMAIN_TRADE_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_TRADE_INSTRUMENT_HPP

#include "ores.trading.api/domain/bond_instrument.hpp"
#include "ores.trading.api/domain/commodity_instrument.hpp"
#include "ores.trading.api/domain/credit_instrument.hpp"
#include "ores.trading.api/domain/equity_instrument_variant.hpp"
#include "ores.trading.api/domain/fx_instrument_variant.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/domain/scripted_instrument.hpp"
#include <variant>

namespace ores::trading::domain {

/**
 * @brief Unified instrument carrier used in both import and export paths.
 *
 * std::monostate — trade has no instrument (unsupported type or new trade).
 * Families with associated leg collections (swap, composite) use with_legs<>
 * wrappers. Families with multiple sub-types (FX, equity, rates) use their
 * inner variant directly. Single-type families carry the domain struct directly.
 */
using trade_instrument = std::variant<std::monostate,
                                      swap_instrument_data,
                                      fx_instrument_variant,
                                      bond_instrument,
                                      credit_instrument,
                                      equity_instrument_variant,
                                      commodity_instrument,
                                      composite_instrument_data,
                                      scripted_instrument>;

inline void
stamp_ids(trade_instrument& ti, boost::uuids::uuid instrument_id, boost::uuids::uuid trade_id) {
    std::visit(
        [&]<typename T>(T& v) {
            if constexpr (!std::is_same_v<T, std::monostate>)
                stamp_ids(v, instrument_id, trade_id);
        },
        ti);
}

} // namespace ores::trading::domain

#endif
