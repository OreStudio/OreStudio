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
#ifndef ORES_TRADING_DOMAIN_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_INSTRUMENT_HPP

#include <concepts>
#include <vector>
#include <variant>
#include <boost/uuid/uuid.hpp>
#include "ores.trading.api/domain/rates_instrument_variant.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/composite_instrument.hpp"
#include "ores.trading.api/domain/composite_leg.hpp"

namespace ores::trading::domain {

template<typename T>
concept Instrument = requires(T t) {
    { t.instrument_id } -> std::convertible_to<boost::uuids::uuid>;
    t.trade_id;
};

template<typename T, typename Leg>
struct with_legs {
    T instrument;
    std::vector<Leg> legs;
};

using swap_instrument_data      = with_legs<rates_instrument_variant, swap_leg>;
using composite_instrument_data = with_legs<composite_instrument, composite_leg>;

template<Instrument T>
void stamp_ids(T& instr,
               boost::uuids::uuid instrument_id,
               boost::uuids::uuid trade_id) {
    instr.instrument_id = instrument_id;
    instr.trade_id      = trade_id;
}

template<typename... Ts>
    requires (Instrument<Ts> && ...)
void stamp_ids(std::variant<Ts...>& v,
               boost::uuids::uuid instrument_id,
               boost::uuids::uuid trade_id) {
    std::visit([&]<Instrument I>(I& instr) {
        stamp_ids(instr, instrument_id, trade_id);
    }, v);
}

template<typename T, typename Leg>
void stamp_ids(with_legs<T, Leg>& data,
               boost::uuids::uuid instrument_id,
               boost::uuids::uuid trade_id) {
    stamp_ids(data.instrument, instrument_id, trade_id);
    for (auto& leg : data.legs)
        leg.instrument_id = instrument_id;
}

} // namespace ores::trading::domain

#endif
