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
#include "ores.trading.api/domain/equity_basket_constituent.hpp"
#include "ores.trading.api/domain/equity_instrument_variant.hpp"
#include "ores.trading.api/domain/equity_position_option_underlying.hpp"
#include "ores.trading.api/domain/flexi_swap_lower_notional.hpp"
#include "ores.trading.api/domain/fx_accumulator_instrument.hpp"
#include "ores.trading.api/domain/fx_asian_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_barrier_option_instrument.hpp"
#include "ores.trading.api/domain/fx_digital_option_instrument.hpp"
#include "ores.trading.api/domain/fx_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_vanilla_option_instrument.hpp"
#include "ores.trading.api/domain/fx_variance_swap_instrument.hpp"
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
#include "ores.trading.api/domain/range_bound.hpp"
#include "ores.trading.api/domain/rate_instrument.hpp"
#include "ores.trading.api/domain/rates_fact_variant.hpp"
#include "ores.trading.api/domain/risk_participation_agreement_instrument.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/swap_leg_amount.hpp"
#include "ores.trading.api/domain/swap_leg_rate.hpp"
#include "ores.trading.api/domain/synthetic_cdo_instrument.hpp"
#include <boost/uuid/uuid.hpp>
#include <concepts>
#include <cstddef>
#include <functional>
#include <optional>
#include <tuple>
#include <type_traits>
#include <utility>
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

// The credit fact types, one per concrete product, and the array each lives
// in. The two tuples together are the one home of the type-to-array map: a new
// credit product is one entry in each, and every reader below follows them
// instead of repeating the list. A static assertion proves they line up.
using credit_fact_types = std::tuple<credit_default_swap_instrument,
                                     index_credit_default_swap_instrument,
                                     index_credit_default_swap_option_instrument,
                                     synthetic_cdo_instrument,
                                     credit_linked_swap_instrument,
                                     risk_participation_agreement_instrument>;

inline constexpr auto credit_fact_arrays =
    std::tuple{&credit_instrument_data::credit_default_swap_instruments,
               &credit_instrument_data::index_credit_default_swap_instruments,
               &credit_instrument_data::index_credit_default_swap_option_instruments,
               &credit_instrument_data::synthetic_cdo_instruments,
               &credit_instrument_data::credit_linked_swap_instruments,
               &credit_instrument_data::risk_participation_agreement_instruments};

namespace detail {

template <typename T, typename Tuple>
struct tuple_contains : std::false_type {};

template <typename T, typename... Ts>
struct tuple_contains<T, std::tuple<Ts...>> : std::bool_constant<(std::is_same_v<T, Ts> || ...)> {};

// The array a member pointer points into, so the two tuples can be proved to
// line up instead of being assumed to.
template <typename T>
struct member_value;

template <typename Value, typename Owner>
struct member_value<Value Owner::*> {
    using type = Value;
};

template <std::size_t... I>
constexpr bool arrays_match_types(std::index_sequence<I...>) {
    return ((std::is_same_v<typename member_value<std::decay_t<decltype(std::get<I>(
                                credit_fact_arrays))>>::type::value_type,
                            std::tuple_element_t<I, credit_fact_types>>) &&
            ...);
}

} // namespace detail

// A credit fact is one of the modelled products. A type that is not one is
// refused at the call site rather than quietly reaching another product's
// array.
template <typename T>
inline constexpr bool is_credit_fact_type = detail::tuple_contains<T, credit_fact_types>::value;

static_assert(
    detail::arrays_match_types(std::make_index_sequence<std::tuple_size_v<credit_fact_types>>{}));

// Applies a callable to every credit fact array, const or mutable. The callable
// receives one array and deduces its row type, so it picks the array it wants
// with if constexpr and needs no discriminator.
template <typename F>
void for_each_credit_fact_array(const credit_instrument_data& data, F&& f) {
    std::apply([&f, &data](auto... member) { (std::invoke(f, data.*member), ...); },
               credit_fact_arrays);
}

template <typename F>
void for_each_credit_fact_array(credit_instrument_data& data, F&& f) {
    std::apply([&f, &data](auto... member) { (std::invoke(f, data.*member), ...); },
               credit_fact_arrays);
}

// The typed array a credit fact type lives in. A reader that names the fact
// type once reaches its own array, so no reader needs a discriminator.
template <typename T>
[[nodiscard]] const std::vector<T>& fact_rows(const credit_instrument_data& data) {
    static_assert(is_credit_fact_type<T>,
                  "not a credit fact type: add it to credit_fact_types and credit_fact_arrays");
    const std::vector<T>* found = nullptr;
    for_each_credit_fact_array(data, [&found](const auto& rows) {
        if constexpr (std::is_same_v<typename std::decay_t<decltype(rows)>::value_type, T>)
            found = &rows;
    });
    return *found;
}

// Drives a per-product visitor over the credit fact arrays and reports which
// array answered. A credit carrier states exactly one fact row per trade,
// joined by trade id, so the visitor sees the one row that is. An empty result
// means the carrier states no fact at all, which a caller has to tell apart
// from a visitor that answered false.
template <typename Visitor>
auto visit_credit_facts(Visitor&& visitor, const credit_instrument_data& data) {
    using Result = std::invoke_result_t<Visitor, const credit_default_swap_instrument&>;
    static_assert(!std::is_void_v<Result>,
                  "a credit fact visitor returns a value, so a carrier with no fact is reported "
                  "as an empty result rather than as silence");
    std::optional<Result> result;
    for_each_credit_fact_array(data, [&](const auto& rows) {
        if (!result && !rows.empty())
            result = std::invoke(visitor, rows.front());
    });
    return result;
}

// An equity position states an entry collection beside the instrument that
// owns it, so the carrier holds the two together and each entry travels
// with its instrument. An equity leaf that states no position leaves the
// collection empty.
struct equity_instrument_data {
    equity_instrument_variant instrument;
    std::vector<equity_position_option_underlying> underlyings;
    /**
     * @brief The constituents of a worst-of basket, in the document's order.
     *
     * A plain swap states one underlying on its facts and leaves this empty; a
     * basket swap states the list here and leaves the fact null. The order is
     * the document's, carried by each row's ordinal.
     */
    std::vector<equity_basket_constituent> basket_constituents;
};

/**
 * @brief The FX family's instrument rows: one typed array per product.
 *
 * The family states seven products and no family header, so each product's
 * table holds the whole product. The carrier is therefore one typed array per
 * product, joined by trade id, rather than an aggregate with a discriminator:
 * the array an element sits in states its type, so nothing needs a tag, and a
 * child the family gains is one more array here. A trade that states no FX
 * instrument leaves every array empty. A product's children, such as an
 * accumulator's range bounds, sit beside the product arrays in their own
 * arrays, keyed by the same trade.
 */
struct fx_instrument_data {
    /**
     * @brief The fx_forward_instrument rows: FxForward and the FxSwap near leg.
     */
    std::vector<fx_forward_instrument> forwards;

    /**
     * @brief The fx_vanilla_option_instrument rows: FxOption.
     */
    std::vector<fx_vanilla_option_instrument> vanilla_options;

    /**
     * @brief The fx_barrier_option_instrument rows: the five barrier codes.
     */
    std::vector<fx_barrier_option_instrument> barrier_options;

    /**
     * @brief The fx_digital_option_instrument rows: digital, touch and double touch.
     */
    std::vector<fx_digital_option_instrument> digital_options;

    /**
     * @brief The fx_asian_forward_instrument rows: FxAverageForward and FxTaRF.
     */
    std::vector<fx_asian_forward_instrument> asian_forwards;

    /**
     * @brief The fx_accumulator_instrument rows: FxAccumulator.
     */
    std::vector<fx_accumulator_instrument> accumulators;

    /**
     * @brief The fx_variance_swap_instrument rows: FxVarianceSwap.
     */
    std::vector<fx_variance_swap_instrument> variance_swaps;

    /**
     * @brief The range_bound rows: the RangeBounds list an accumulator states.
     *
     * A child of the product rather than a product, so it states no type and
     * the visitor that picks the product never reads it.
     */
    std::vector<range_bound> range_bounds;

    /**
     * @brief Value equality.
     *
     * A carrier is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a carrier without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const fx_instrument_data&, const fx_instrument_data&) = default;
};

namespace detail {

/**
 * @brief The family's arrays, in the order the family states its products.
 *
 * Every walk over the arrays reads this one list, so a product the family
 * gains is one more entry here and in the batch's pairing of the same seven,
 * and no walk needs changing.
 */
inline constexpr auto fx_instrument_arrays = std::tuple{&fx_instrument_data::forwards,
                                                        &fx_instrument_data::vanilla_options,
                                                        &fx_instrument_data::barrier_options,
                                                        &fx_instrument_data::digital_options,
                                                        &fx_instrument_data::asian_forwards,
                                                        &fx_instrument_data::accumulators,
                                                        &fx_instrument_data::variance_swaps};

/**
 * @brief The family's child arrays: rows a product owns rather than rows a
 * product is.
 *
 * A child states no product, so no walk that picks the product reads this
 * list. Every walk that stamps or joins the carrier reads it, because a child
 * row is keyed by the same trade the product row is.
 */
inline constexpr auto fx_instrument_child_arrays = std::tuple{&fx_instrument_data::range_bounds};

/**
 * @brief Calls a function on the first row the carrier holds, and on no other.
 *
 * @return true when the carrier held a row.
 */
template <typename Fn>
bool visit_first_fx_product(const fx_instrument_data& data, Fn&& fn) {
    bool called = false;
    const auto visit_one = [&](const auto& rows) {
        if (called || rows.empty())
            return;
        fn(rows.front());
        called = true;
    };
    std::apply([&](const auto... member) { (visit_one(data.*member), ...); }, fx_instrument_arrays);
    return called;
}

} // namespace detail

/**
 * @brief Calls a visitor on the one row an FX carrier holds for a trade.
 *
 * @details The carrier states a product by the array its row sits in, so the
 * first array that holds a row decides and the rest are skipped. The trade id
 * is the row's key, so the join states one row per trade, and a carrier that a
 * second row reaches states a fault that the instrument table does not allow.
 *
 * The visitor's own return type comes back unchanged, so a caller that asks a
 * question gets its answer and a caller that only acts gets nothing back. A
 * carrier that holds no row calls nothing and returns a default-constructed
 * result. Neither path builds such a carrier: the import path pushes a row
 * before it answers, and the export path answers only for a trade the batch
 * holds a row for.
 */
template <typename Visitor>
auto visit_fx_products(Visitor visitor, const fx_instrument_data& data) {
    using Result = std::invoke_result_t<Visitor, const fx_forward_instrument&>;
    if constexpr (std::is_void_v<Result>) {
        detail::visit_first_fx_product(data, visitor);
    } else {
        std::optional<Result> result;
        detail::visit_first_fx_product(data,
                                       [&](const auto& row) { result.emplace(visitor(row)); });
        return result.value_or(Result{});
    }
}

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
    for_each_credit_fact_array(data, [&](auto& rows) {
        for (auto& fact : rows)
            stamp_ids(fact, trade_id, activity_id);
    });
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

inline void stamp_ids(fx_instrument_data& data,
                      boost::uuids::uuid trade_id,
                      boost::uuids::uuid activity_id = {}) {
    const auto stamp_one = [&](auto& rows) {
        for (auto& row : rows) {
            stamp_ids(row, trade_id, activity_id);
        }
    };
    std::apply([&](const auto... member) { (stamp_one(data.*member), ...); },
               std::tuple_cat(detail::fx_instrument_arrays, detail::fx_instrument_child_arrays));
}

} // namespace ores::trading::domain

#endif
