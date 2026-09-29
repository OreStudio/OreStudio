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

#include "ores.trading.api/domain/callable_swap_call_date.hpp"
#include "ores.trading.api/domain/commodity_basket_constituent.hpp"
#include "ores.trading.api/domain/commodity_instrument.hpp"
#include "ores.trading.api/domain/composite_instrument.hpp"
#include "ores.trading.api/domain/composite_leg.hpp"
#include "ores.trading.api/domain/equity_instrument_variant.hpp"
#include "ores.trading.api/domain/equity_position_option_underlying.hpp"
#include "ores.trading.api/domain/rates_instrument_variant.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include <boost/uuid/uuid.hpp>
#include <concepts>
#include <type_traits>
#include <variant>
#include <vector>

namespace ores::trading::domain {

// Every instrument type carries instrument_identity + audit_record; the
// flat-field era (and the FlatInstrument concept that bridged the
// incremental migration in tasks 12-15) is over.
template <typename T>
concept Instrument = requires(T t) {
    { t.identity.trade_id } -> std::convertible_to<boost::uuids::uuid>;
};

// Retained as an alias for call sites written during the migration.
template <typename T>
concept NestedInstrument = Instrument<T>;

template <typename T, typename Leg>
struct with_legs {
    T instrument;
    std::vector<Leg> legs;
};

// The rates family states a leg collection and, for a callable swap, the
// schedule of dates its owner may exercise the call on. Both are
// collections that belong to the instrument, so the family carrier holds
// them beside each other and each call date travels with the instrument
// it belongs to. A leaf that states no schedule leaves the collection
// empty.
struct swap_instrument_data {
    rates_instrument_variant instrument;
    std::vector<swap_leg> legs;
    std::vector<callable_swap_call_date> call_dates;
};

using composite_instrument_data = with_legs<composite_instrument, composite_leg>;

// A commodity basket states a constituent collection beside the instrument
// that owns it, so the carrier holds the two together and each constituent
// travels with its instrument. A commodity product that states no basket
// leaves the collection empty.
struct commodity_instrument_data {
    commodity_instrument instrument;
    std::vector<commodity_basket_constituent> constituents;
};

// An equity position states an entry collection beside the instrument that
// owns it, so the carrier holds the two together and each entry travels
// with its instrument. An equity leaf that states no position leaves the
// collection empty.
struct equity_instrument_data {
    equity_instrument_variant instrument;
    std::vector<equity_position_option_underlying> underlyings;
};

// The trade id is the instrument's own key, so there is one id to stamp
// rather than a trade id and an instrument id that can disagree.
template <Instrument T>
void stamp_ids(T& instr, boost::uuids::uuid trade_id) {
    instr.identity.trade_id = trade_id;
}

template <typename... Ts>
    requires(Instrument<Ts> && ...)
void stamp_ids(std::variant<Ts...>& v, boost::uuids::uuid trade_id) {
    std::visit([&](auto& instr) { stamp_ids(instr, trade_id); }, v);
}

template <typename T, typename Leg>
void stamp_ids(with_legs<T, Leg>& data, boost::uuids::uuid trade_id) {
    stamp_ids(data.instrument, trade_id);
    for (auto& leg : data.legs)
        leg.identity.trade_id = trade_id;
}

inline void stamp_ids(swap_instrument_data& data, boost::uuids::uuid trade_id) {
    stamp_ids(data.instrument, trade_id);
    for (auto& leg : data.legs)
        leg.identity.trade_id = trade_id;
    for (auto& call_date : data.call_dates)
        call_date.trade_id = trade_id;
}

inline void stamp_ids(commodity_instrument_data& data, boost::uuids::uuid trade_id) {
    stamp_ids(data.instrument, trade_id);
    for (auto& constituent : data.constituents)
        constituent.trade_id = trade_id;
}

inline void stamp_ids(equity_instrument_data& data, boost::uuids::uuid trade_id) {
    stamp_ids(data.instrument, trade_id);
    for (auto& underlying : data.underlyings)
        underlying.trade_id = trade_id;
}

} // namespace ores::trading::domain

#endif
