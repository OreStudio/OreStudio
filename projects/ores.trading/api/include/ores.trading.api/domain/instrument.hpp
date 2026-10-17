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

#include "ores.trading.api/domain/balance_guaranteed_swap_tranche.hpp"
#include "ores.trading.api/domain/balance_guaranteed_swap_tranche_notional.hpp"
#include "ores.trading.api/domain/callable_swap_call_date.hpp"
#include "ores.trading.api/domain/commodity_basket_constituent.hpp"
#include "ores.trading.api/domain/commodity_instrument.hpp"
#include "ores.trading.api/domain/composite_instrument.hpp"
#include "ores.trading.api/domain/composite_leg.hpp"
#include "ores.trading.api/domain/credit_basket_constituent.hpp"
#include "ores.trading.api/domain/credit_default_swap_instrument.hpp"
#include "ores.trading.api/domain/credit_instrument.hpp"
#include "ores.trading.api/domain/credit_linked_swap_instrument.hpp"
#include "ores.trading.api/domain/equity_instrument_variant.hpp"
#include "ores.trading.api/domain/equity_position_option_underlying.hpp"
#include "ores.trading.api/domain/flexi_swap_lower_notional.hpp"
#include "ores.trading.api/domain/index_credit_default_swap_instrument.hpp"
#include "ores.trading.api/domain/index_credit_default_swap_option_instrument.hpp"
#include "ores.trading.api/domain/instrument_option.hpp"
#include "ores.trading.api/domain/instrument_option_exercise_fee.hpp"
#include "ores.trading.api/domain/instrument_option_exercise_price.hpp"
#include "ores.trading.api/domain/instrument_option_payment_date.hpp"
#include "ores.trading.api/domain/instrument_option_premium.hpp"
#include "ores.trading.api/domain/instrument_schedule.hpp"
#include "ores.trading.api/domain/instrument_schedule_date.hpp"
#include "ores.trading.api/domain/instrument_strike.hpp"
#include "ores.trading.api/domain/rate_instrument.hpp"
#include "ores.trading.api/domain/rates_fact_variant.hpp"
#include "ores.trading.api/domain/risk_participation_agreement_instrument.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/swap_leg_amount.hpp"
#include "ores.trading.api/domain/swap_leg_rate.hpp"
#include "ores.trading.api/domain/synthetic_cdo_instrument.hpp"
#include <boost/uuid/uuid.hpp>
#include <concepts>
#include <type_traits>
#include <variant>
#include <vector>

namespace ores::trading::domain {

// The rates family's header and every other instrument type carries
// instrument_identity, so the trade id is reached through it. The header is
// the one row of the family that does: the fact tables carry the trade id
// flat and are keyed by TradeKeyed instead.
template <typename T>
concept Instrument = requires(T t) {
    { t.identity.trade_id } -> std::convertible_to<boost::uuids::uuid>;
};

// A rates fact row: the product's own fields, keyed by the flat trade id
// that joins it to the header.
template <typename T>
concept TradeKeyed = requires(T t) {
    { t.trade_id } -> std::convertible_to<boost::uuids::uuid>;
    { t.trade_activity_id } -> std::convertible_to<boost::uuids::uuid>;
};

// Retained as an alias for call sites written during the migration.
template <typename T>
concept NestedInstrument = Instrument<T>;

template <typename T, typename Leg>
struct with_legs {
    T instrument;
    std::vector<Leg> legs;
};

// The rates family states one header row, the product's own fact row, a leg
// collection, and for a callable swap the schedule of dates its owner may
// exercise the call on. The header holds the identity, the dates and the
// description; the fact variant holds the fields that belong to the product
// alone. Both are keyed by the same trade, so a rates instrument in memory
// is the header plus its fact plus its children. A leaf that states no
// schedule leaves the collection empty.
//
// The family writes to the shared children the bond pilot built: a
// cap/floor states its strikes and its underlying swap's schedules and a
// swaption states an option block. Those rows are the shared entities
// themselves, keyed by the flat trade id, so the carrier holds them beside
// its own legs. A leaf that states none leaves the collection empty.
struct swap_instrument_data {
    rate_instrument header;
    rates_fact_variant facts;
    std::vector<swap_leg> legs;
    std::vector<swap_leg_amount> leg_amounts;
    std::vector<swap_leg_rate> leg_rates;
    std::vector<callable_swap_call_date> call_dates;
    std::vector<instrument_strike> strikes;
    std::vector<instrument_schedule> schedules;
    std::vector<instrument_schedule_date> schedule_dates;
    std::vector<instrument_option> options;
    std::vector<instrument_option_premium> option_premiums;
    std::vector<instrument_option_exercise_fee> option_exercise_fees;
    std::vector<instrument_option_payment_date> option_payment_dates;
    std::vector<instrument_option_exercise_price> option_exercise_prices;
    std::vector<flexi_swap_lower_notional> lower_notionals;
    std::vector<balance_guaranteed_swap_tranche> tranches;
    std::vector<balance_guaranteed_swap_tranche_notional> tranche_notionals;
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

// A credit trade states its legs as rows of the shared leg tables, and an index
// swap or a synthetic CDO states a basket of names beside the instrument. The
// header is the family's routed instrument and the product's own fields are its
// typed fact arrays, one array per product, each row joined to the header by the
// trade id it carries flat. An index option states a strike and its exercise
// dates in the shared strike and schedule tables. The carrier holds the header
// with those rows, so each travels with the trade it belongs to. A credit
// product that states none of them leaves the collection empty.
struct credit_instrument_data {
    credit_instrument instrument;
    std::vector<credit_default_swap_instrument> credit_default_swap_instruments;
    std::vector<index_credit_default_swap_instrument> index_credit_default_swap_instruments;
    std::vector<index_credit_default_swap_option_instrument>
        index_credit_default_swap_option_instruments;
    std::vector<synthetic_cdo_instrument> synthetic_cdo_instruments;
    std::vector<credit_linked_swap_instrument> credit_linked_swap_instruments;
    std::vector<risk_participation_agreement_instrument> risk_participation_agreement_instruments;
    std::vector<swap_leg> legs;
    std::vector<swap_leg_amount> leg_amounts;
    std::vector<swap_leg_rate> leg_rates;
    std::vector<credit_basket_constituent> constituents;
    std::vector<instrument_strike> strikes;
    std::vector<instrument_schedule> schedules;
    std::vector<instrument_schedule_date> schedule_dates;
    std::vector<instrument_option> options;
    std::vector<instrument_option_premium> option_premiums;
    std::vector<instrument_option_exercise_fee> option_exercise_fees;
    std::vector<instrument_option_payment_date> option_payment_dates;
    std::vector<instrument_option_exercise_price> option_exercise_prices;
    // An index option wraps an index swap, which has fact fields of its own.
    // The option's fact row is one of the carrier's fact arrays; the wrapped
    // swap's row is here. A product that wraps no swap leaves it empty.
    std::optional<index_credit_default_swap_instrument> wrapped_index_swap;
};

// The typed array a credit fact type lives in. A reader that names the fact
// type once reaches its own array, so no reader needs a discriminator and the
// type-to-array map has one home.
template <typename T>
[[nodiscard]] const std::vector<T>& fact_rows(const credit_instrument_data& data) {
    if constexpr (std::is_same_v<T, credit_default_swap_instrument>)
        return data.credit_default_swap_instruments;
    else if constexpr (std::is_same_v<T, index_credit_default_swap_instrument>)
        return data.index_credit_default_swap_instruments;
    else if constexpr (std::is_same_v<T, index_credit_default_swap_option_instrument>)
        return data.index_credit_default_swap_option_instruments;
    else if constexpr (std::is_same_v<T, synthetic_cdo_instrument>)
        return data.synthetic_cdo_instruments;
    else if constexpr (std::is_same_v<T, credit_linked_swap_instrument>)
        return data.credit_linked_swap_instruments;
    else
        return data.risk_participation_agreement_instruments;
}

// Drives a per-product visitor over the credit fact arrays. A credit carrier
// states exactly one fact row per trade, joined by trade id, so the visitor
// sees the one row that is. A carrier that states none calls nothing, and the
// result is the visitor's own, so a caller that asks a question gets an
// answer.
template <typename Visitor>
auto visit_credit_facts(Visitor&& visitor, const credit_instrument_data& data) {
    using Result = std::invoke_result_t<Visitor, const credit_default_swap_instrument&>;
    if constexpr (std::is_void_v<Result>) {
        if (!data.credit_default_swap_instruments.empty())
            visitor(data.credit_default_swap_instruments.front());
        else if (!data.index_credit_default_swap_instruments.empty())
            visitor(data.index_credit_default_swap_instruments.front());
        else if (!data.index_credit_default_swap_option_instruments.empty())
            visitor(data.index_credit_default_swap_option_instruments.front());
        else if (!data.synthetic_cdo_instruments.empty())
            visitor(data.synthetic_cdo_instruments.front());
        else if (!data.credit_linked_swap_instruments.empty())
            visitor(data.credit_linked_swap_instruments.front());
        else if (!data.risk_participation_agreement_instruments.empty())
            visitor(data.risk_participation_agreement_instruments.front());
    } else {
        if (!data.credit_default_swap_instruments.empty())
            return visitor(data.credit_default_swap_instruments.front());
        if (!data.index_credit_default_swap_instruments.empty())
            return visitor(data.index_credit_default_swap_instruments.front());
        if (!data.index_credit_default_swap_option_instruments.empty())
            return visitor(data.index_credit_default_swap_option_instruments.front());
        if (!data.synthetic_cdo_instruments.empty())
            return visitor(data.synthetic_cdo_instruments.front());
        if (!data.credit_linked_swap_instruments.empty())
            return visitor(data.credit_linked_swap_instruments.front());
        if (!data.risk_participation_agreement_instruments.empty())
            return visitor(data.risk_participation_agreement_instruments.front());
        return Result{};
    }
}

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
void stamp_ids(T& instr, boost::uuids::uuid trade_id, boost::uuids::uuid activity_id = {}) {
    instr.identity.trade_id = trade_id;
    instr.identity.trade_activity_id = activity_id;
}

template <TradeKeyed T>
void stamp_ids(T& fact, boost::uuids::uuid trade_id, boost::uuids::uuid activity_id = {}) {
    fact.trade_id = trade_id;
    fact.trade_activity_id = activity_id;
}

template <typename... Ts>
    requires(Instrument<Ts> && ...)
void stamp_ids(std::variant<Ts...>& v,
               boost::uuids::uuid trade_id,
               boost::uuids::uuid activity_id = {}) {
    std::visit([&](auto& instr) { stamp_ids(instr, trade_id, activity_id); }, v);
}

template <typename... Ts>
    requires(TradeKeyed<Ts> && ...)
void stamp_ids(std::variant<Ts...>& v,
               boost::uuids::uuid trade_id,
               boost::uuids::uuid activity_id = {}) {
    std::visit([&](auto& fact) { stamp_ids(fact, trade_id, activity_id); }, v);
}

template <typename T, typename Leg>
void stamp_ids(with_legs<T, Leg>& data,
               boost::uuids::uuid trade_id,
               boost::uuids::uuid activity_id = {}) {
    stamp_ids(data.instrument, trade_id, activity_id);
    for (auto& leg : data.legs) {
        leg.identity.trade_id = trade_id;
        leg.identity.trade_activity_id = activity_id;
    }
}

inline void stamp_ids(swap_instrument_data& data,
                      boost::uuids::uuid trade_id,
                      boost::uuids::uuid activity_id = {}) {
    stamp_ids(data.header, trade_id, activity_id);
    stamp_ids(data.facts, trade_id, activity_id);
    for (auto& leg : data.legs) {
        leg.identity.trade_id = trade_id;
        leg.identity.trade_activity_id = activity_id;
    }
    for (auto& amount : data.leg_amounts) {
        stamp_ids(amount, trade_id, activity_id);
    }
    for (auto& rate : data.leg_rates) {
        stamp_ids(rate, trade_id, activity_id);
    }
    for (auto& call_date : data.call_dates) {
        stamp_ids(call_date, trade_id, activity_id);
    }
    for (auto& strike : data.strikes) {
        stamp_ids(strike, trade_id, activity_id);
    }
    for (auto& schedule : data.schedules) {
        stamp_ids(schedule, trade_id, activity_id);
    }
    for (auto& schedule_date : data.schedule_dates) {
        stamp_ids(schedule_date, trade_id, activity_id);
    }
    for (auto& option : data.options) {
        stamp_ids(option, trade_id, activity_id);
    }
    for (auto& premium : data.option_premiums) {
        stamp_ids(premium, trade_id, activity_id);
    }
    for (auto& fee : data.option_exercise_fees) {
        stamp_ids(fee, trade_id, activity_id);
    }
    for (auto& payment_date : data.option_payment_dates) {
        stamp_ids(payment_date, trade_id, activity_id);
    }
    for (auto& exercise_price : data.option_exercise_prices) {
        stamp_ids(exercise_price, trade_id, activity_id);
    }
    for (auto& lower_notional : data.lower_notionals) {
        stamp_ids(lower_notional, trade_id, activity_id);
    }
    for (auto& tranche : data.tranches) {
        stamp_ids(tranche, trade_id, activity_id);
    }
    for (auto& tranche_notional : data.tranche_notionals) {
        stamp_ids(tranche_notional, trade_id, activity_id);
    }
}

inline void stamp_ids(commodity_instrument_data& data,
                      boost::uuids::uuid trade_id,
                      boost::uuids::uuid activity_id = {}) {
    stamp_ids(data.instrument, trade_id, activity_id);
    for (auto& constituent : data.constituents) {
        constituent.trade_id = trade_id;
        constituent.trade_activity_id = activity_id;
    }
}

inline void stamp_ids(credit_instrument_data& data,
                      boost::uuids::uuid trade_id,
                      boost::uuids::uuid activity_id = {}) {
    stamp_ids(data.instrument, trade_id, activity_id);
    for (auto& leg : data.legs) {
        leg.identity.trade_id = trade_id;
        leg.identity.trade_activity_id = activity_id;
    }
    for (auto& amount : data.leg_amounts) {
        stamp_ids(amount, trade_id, activity_id);
    }
    for (auto& rate : data.leg_rates) {
        stamp_ids(rate, trade_id, activity_id);
    }
    for (auto& constituent : data.constituents) {
        constituent.trade_id = trade_id;
        constituent.trade_activity_id = activity_id;
    }
    for (auto& fact : data.credit_default_swap_instruments) {
        stamp_ids(fact, trade_id, activity_id);
    }
    for (auto& fact : data.index_credit_default_swap_instruments) {
        stamp_ids(fact, trade_id, activity_id);
    }
    for (auto& fact : data.index_credit_default_swap_option_instruments) {
        stamp_ids(fact, trade_id, activity_id);
    }
    for (auto& fact : data.synthetic_cdo_instruments) {
        stamp_ids(fact, trade_id, activity_id);
    }
    for (auto& fact : data.credit_linked_swap_instruments) {
        stamp_ids(fact, trade_id, activity_id);
    }
    for (auto& fact : data.risk_participation_agreement_instruments) {
        stamp_ids(fact, trade_id, activity_id);
    }
    for (auto& strike : data.strikes) {
        stamp_ids(strike, trade_id, activity_id);
    }
    for (auto& schedule : data.schedules) {
        stamp_ids(schedule, trade_id, activity_id);
    }
    for (auto& schedule_date : data.schedule_dates) {
        stamp_ids(schedule_date, trade_id, activity_id);
    }
    for (auto& option : data.options) {
        stamp_ids(option, trade_id, activity_id);
    }
    for (auto& premium : data.option_premiums) {
        stamp_ids(premium, trade_id, activity_id);
    }
    for (auto& fee : data.option_exercise_fees) {
        stamp_ids(fee, trade_id, activity_id);
    }
    for (auto& payment_date : data.option_payment_dates) {
        stamp_ids(payment_date, trade_id, activity_id);
    }
    for (auto& exercise_price : data.option_exercise_prices) {
        stamp_ids(exercise_price, trade_id, activity_id);
    }
    if (data.wrapped_index_swap)
        stamp_ids(*data.wrapped_index_swap, trade_id, activity_id);
}

inline void stamp_ids(equity_instrument_data& data,
                      boost::uuids::uuid trade_id,
                      boost::uuids::uuid activity_id = {}) {
    stamp_ids(data.instrument, trade_id, activity_id);
    for (auto& underlying : data.underlyings) {
        underlying.trade_id = trade_id;
        underlying.trade_activity_id = activity_id;
    }
}

} // namespace ores::trading::domain

#endif
