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
#include "ores.qt/parse_trade_instrument.hpp"
#include "ores.qt/IInstrumentFormPopulator.hpp"
#include "ores.logging/make_logger.hpp"

namespace {

using namespace ores::logging;
namespace td = ores::trading::domain;

inline std::string_view logger_name = "ores.qt.parse_trade_instrument";
[[nodiscard]] static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

// Phase 1: envelope — rfl silently skips the "instrument" JSON key.
struct response_envelope {
    bool success = false;
    std::string message;
    td::trade trade;
};

// Phase 2: one named wrapper per concrete leaf type.
// Named wrappers are required because C++ does not permit struct definitions
// inside template argument lists (e.g. rfl::json::read<struct{...}> is ill-formed).
//
// Flat types (bond, credit, commodity, scripted, fx_*, equity_*):
//   trade_instrument holds the concrete type directly (or within a sub-variant).
//   The "instrument" key in the response JSON maps directly to the leaf struct's fields.
//   Structure: response["instrument"] = {concrete_instrument_fields}
//
// with_legs types (swap, composite):
//   trade_instrument holds swap_instrument_data or composite_instrument_data, both of
//   which are with_legs<VariantOrStruct, Leg> = {T instrument; vector<Leg> legs;}.
//   The "instrument" key in the response JSON maps to the with_legs struct, which itself
//   has an "instrument" field containing the concrete type.
//   Structure: response["instrument"] = {"instrument": {concrete_fields}, "legs": [...]}
//   The *_data intermediate struct captures this inner nesting.

// Flat types
struct bond_wrapper      { td::bond_instrument      instrument; };
struct credit_wrapper    { td::credit_instrument    instrument; };
struct commodity_wrapper { td::commodity_instrument instrument; };
struct scripted_wrapper  { td::scripted_instrument  instrument; };

// Composite (with composite legs) — two-level wrapper: outer captures with_legs structure,
// inner captures the concrete composite_instrument.
struct composite_data {
    td::composite_instrument             instrument;
    std::vector<td::composite_leg>       legs;
};
struct composite_wrapper { composite_data instrument; };

// Rates / swap types (with swap legs) — same two-level pattern as composite.
struct fra_data { td::fra_instrument instrument; std::vector<td::swap_leg> legs; };
struct fra_wrapper { fra_data instrument; };

struct vanilla_swap_data { td::vanilla_swap_instrument instrument; std::vector<td::swap_leg> legs; };
struct vanilla_swap_wrapper { vanilla_swap_data instrument; };

struct cap_floor_data { td::cap_floor_instrument instrument; std::vector<td::swap_leg> legs; };
struct cap_floor_wrapper { cap_floor_data instrument; };

struct swaption_data { td::swaption_instrument instrument; std::vector<td::swap_leg> legs; };
struct swaption_wrapper { swaption_data instrument; };

struct bgs_data { td::balance_guaranteed_swap_instrument instrument; std::vector<td::swap_leg> legs; };
struct bgs_wrapper { bgs_data instrument; };

struct callable_swap_data { td::callable_swap_instrument instrument; std::vector<td::swap_leg> legs; };
struct callable_swap_wrapper { callable_swap_data instrument; };

struct knock_out_swap_data { td::knock_out_swap_instrument instrument; std::vector<td::swap_leg> legs; };
struct knock_out_swap_wrapper { knock_out_swap_data instrument; };

struct inflation_swap_data { td::inflation_swap_instrument instrument; std::vector<td::swap_leg> legs; };
struct inflation_swap_wrapper { inflation_swap_data instrument; };

struct rpa_data { td::rpa_instrument instrument; std::vector<td::swap_leg> legs; };
struct rpa_wrapper { rpa_data instrument; };

// FX types (direct alternatives in fx_instrument_variant — no extra nesting)
struct fx_forward_wrapper        { td::fx_forward_instrument        instrument; };
struct fx_vanilla_option_wrapper { td::fx_vanilla_option_instrument instrument; };
struct fx_barrier_option_wrapper { td::fx_barrier_option_instrument instrument; };
struct fx_digital_option_wrapper { td::fx_digital_option_instrument instrument; };
struct fx_asian_forward_wrapper  { td::fx_asian_forward_instrument  instrument; };
struct fx_accumulator_wrapper    { td::fx_accumulator_instrument    instrument; };
struct fx_variance_swap_wrapper  { td::fx_variance_swap_instrument  instrument; };

// Equity types (direct alternatives in equity_instrument_variant — no extra nesting)
struct eq_option_wrapper    { td::equity_option_instrument          instrument; };
struct eq_forward_wrapper   { td::equity_forward_instrument         instrument; };
struct eq_swap_wrapper      { td::equity_swap_instrument            instrument; };
struct eq_var_swap_wrapper  { td::equity_variance_swap_instrument   instrument; };
struct eq_barrier_wrapper   { td::equity_barrier_option_instrument  instrument; };
struct eq_asian_wrapper     { td::equity_asian_option_instrument    instrument; };
struct eq_digital_wrapper   { td::equity_digital_option_instrument  instrument; };
struct eq_accum_wrapper     { td::equity_accumulator_instrument     instrument; };
struct eq_position_wrapper  { td::equity_position_instrument        instrument; };

} // namespace

namespace ores::qt {

std::optional<trading::domain::trade>
parse_trade_instrument(const std::string& raw, IInstrumentFormPopulator& populator) {
    // Phase 1: parse envelope — no variant, no AddTagsToVariants.
    BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: phase 1 parse";
    auto base = rfl::json::read<response_envelope>(raw);
    if (!base) {
        BOOST_LOG_SEV(lg(), error)
            << "getTradeInstrument: deserialise failed: " << base.error().what();
        return std::nullopt;
    }
    if (!base->success) {
        BOOST_LOG_SEV(lg(), error)
            << "getTradeInstrument: server error: " << base->message;
        return std::nullopt;
    }

    const auto& cls = base->trade.classification;
    const auto  pt  = cls.product_type;
    const auto& ttc = cls.trade_type;

    BOOST_LOG_SEV(lg(), debug)
        << "getTradeInstrument: product_type=" << to_string(pt)
        << " trade_type=" << ttc;

    // Phase 2: dispatch on (product_type, trade_type) — read concrete leaf
    // struct directly, no AddTagsToVariants, no trade_instrument variant.
    using ores::trading::domain::product_type;

// Macro for flat types: wrapper has a single "instrument" field at response level.
#define READ_FLAT(WrapperType, call)                                          \
    do {                                                                       \
        auto r = rfl::json::read<WrapperType>(raw);                           \
        if (!r) {                                                              \
            BOOST_LOG_SEV(lg(), error)                                        \
                << "getTradeInstrument: deserialise failed: "                 \
                << r.error().what();                                           \
            return std::nullopt;                                               \
        }                                                                      \
        call;                                                                  \
    } while (0)

// Macro for with_legs types: wrapper has "instrument" → {instrument, legs} nesting.
#define READ_LEGS(WrapperType, call)                                          \
    do {                                                                       \
        auto r = rfl::json::read<WrapperType>(raw);                           \
        if (!r) {                                                              \
            BOOST_LOG_SEV(lg(), error)                                        \
                << "getTradeInstrument: deserialise failed: "                 \
                << r.error().what();                                           \
            return std::nullopt;                                               \
        }                                                                      \
        call;                                                                  \
    } while (0)

    switch (pt) {
    case product_type::bond:
        BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading bond_instrument";
        READ_FLAT(bond_wrapper, populator.populate(r->instrument));
        break;

    case product_type::credit:
        BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading credit_instrument";
        READ_FLAT(credit_wrapper, populator.populate(r->instrument));
        break;

    case product_type::commodity:
        BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading commodity_instrument";
        READ_FLAT(commodity_wrapper, populator.populate(r->instrument));
        break;

    case product_type::scripted:
        BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading scripted_instrument";
        READ_FLAT(scripted_wrapper, populator.populate(r->instrument));
        break;

    case product_type::composite:
        BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading composite_instrument";
        READ_LEGS(composite_wrapper,
                  populator.populate(r->instrument.instrument,
                                     std::move(r->instrument.legs)));
        break;

    case product_type::swap:
        if (ttc == "ForwardRateAgreement") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fra_instrument";
            READ_LEGS(fra_wrapper,
                      populator.populate(r->instrument.instrument,
                                         std::move(r->instrument.legs)));
        } else if (ttc == "Swap" || ttc == "CrossCurrencySwap" || ttc == "FlexiSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading vanilla_swap_instrument";
            READ_LEGS(vanilla_swap_wrapper,
                      populator.populate(r->instrument.instrument,
                                         std::move(r->instrument.legs)));
        } else if (ttc == "CapFloor") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading cap_floor_instrument";
            READ_LEGS(cap_floor_wrapper,
                      populator.populate(r->instrument.instrument,
                                         std::move(r->instrument.legs)));
        } else if (ttc == "Swaption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading swaption_instrument";
            READ_LEGS(swaption_wrapper,
                      populator.populate(r->instrument.instrument,
                                         std::move(r->instrument.legs)));
        } else if (ttc == "BalanceGuaranteedSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading balance_guaranteed_swap_instrument";
            READ_LEGS(bgs_wrapper,
                      populator.populate(r->instrument.instrument,
                                         std::move(r->instrument.legs)));
        } else if (ttc == "CallableSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading callable_swap_instrument";
            READ_LEGS(callable_swap_wrapper,
                      populator.populate(r->instrument.instrument,
                                         std::move(r->instrument.legs)));
        } else if (ttc == "KnockOutSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading knock_out_swap_instrument";
            READ_LEGS(knock_out_swap_wrapper,
                      populator.populate(r->instrument.instrument,
                                         std::move(r->instrument.legs)));
        } else if (ttc == "InflationSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading inflation_swap_instrument";
            READ_LEGS(inflation_swap_wrapper,
                      populator.populate(r->instrument.instrument,
                                         std::move(r->instrument.legs)));
        } else if (ttc == "RiskParticipationAgreement") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading rpa_instrument";
            READ_LEGS(rpa_wrapper,
                      populator.populate(r->instrument.instrument,
                                         std::move(r->instrument.legs)));
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "getTradeInstrument: unknown (product_type, trade_type) — ("
                << to_string(pt) << ", " << ttc << "); returning monostate";
            return std::nullopt;
        }
        break;

    case product_type::fx:
        if (ttc == "FxForward" || ttc == "FxSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_forward_instrument";
            READ_FLAT(fx_forward_wrapper, populator.populate(r->instrument));
        } else if (ttc == "FxOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_vanilla_option_instrument";
            READ_FLAT(fx_vanilla_option_wrapper, populator.populate(r->instrument));
        } else if (ttc == "FxBarrierOption" || ttc == "FxGenericBarrierOption"
                   || ttc == "FxDoubleBarrierOption" || ttc == "FxEuropeanBarrierOption"
                   || ttc == "FxKIKOBarrierOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_barrier_option_instrument";
            READ_FLAT(fx_barrier_option_wrapper, populator.populate(r->instrument));
        } else if (ttc == "FxDigitalOption" || ttc == "FxDigitalBarrierOption"
                   || ttc == "FxTouchOption" || ttc == "FxDoubleTouchOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_digital_option_instrument";
            READ_FLAT(fx_digital_option_wrapper, populator.populate(r->instrument));
        } else if (ttc == "FxAverageForward" || ttc == "FxTaRF") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_asian_forward_instrument";
            READ_FLAT(fx_asian_forward_wrapper, populator.populate(r->instrument));
        } else if (ttc == "FxAccumulator") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_accumulator_instrument";
            READ_FLAT(fx_accumulator_wrapper, populator.populate(r->instrument));
        } else if (ttc == "FxVarianceSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_variance_swap_instrument";
            READ_FLAT(fx_variance_swap_wrapper, populator.populate(r->instrument));
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "getTradeInstrument: unknown (product_type, trade_type) — ("
                << to_string(pt) << ", " << ttc << "); returning monostate";
            return std::nullopt;
        }
        break;

    case product_type::equity:
        if (ttc == "EquityOption" || ttc == "EquityCliquetOption"
            || ttc == "EquityOutperformanceOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_option_instrument";
            READ_FLAT(eq_option_wrapper, populator.populate(r->instrument));
        } else if (ttc == "EquityForward") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_forward_instrument";
            READ_FLAT(eq_forward_wrapper, populator.populate(r->instrument));
        } else if (ttc == "EquitySwap" || ttc == "EquityWorstOfBasketSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_swap_instrument";
            READ_FLAT(eq_swap_wrapper, populator.populate(r->instrument));
        } else if (ttc == "EquityVarianceSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_variance_swap_instrument";
            READ_FLAT(eq_var_swap_wrapper, populator.populate(r->instrument));
        } else if (ttc == "EquityBarrierOption" || ttc == "EquityDoubleBarrierOption"
                   || ttc == "EquityEuropeanBarrierOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_barrier_option_instrument";
            READ_FLAT(eq_barrier_wrapper, populator.populate(r->instrument));
        } else if (ttc == "EquityAsianOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_asian_option_instrument";
            READ_FLAT(eq_asian_wrapper, populator.populate(r->instrument));
        } else if (ttc == "EquityDigitalOption" || ttc == "EquityTouchOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_digital_option_instrument";
            READ_FLAT(eq_digital_wrapper, populator.populate(r->instrument));
        } else if (ttc == "EquityAccumulator" || ttc == "EquityTaRF") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_accumulator_instrument";
            READ_FLAT(eq_accum_wrapper, populator.populate(r->instrument));
        } else if (ttc == "EquityPosition") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_position_instrument";
            READ_FLAT(eq_position_wrapper, populator.populate(r->instrument));
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "getTradeInstrument: unknown (product_type, trade_type) — ("
                << to_string(pt) << ", " << ttc << "); returning monostate";
            return std::nullopt;
        }
        break;

    case product_type::unknown:
    default:
        BOOST_LOG_SEV(lg(), warn)
            << "getTradeInstrument: unknown (product_type, trade_type) — ("
            << to_string(pt) << ", " << ttc << "); returning monostate";
        return std::nullopt;
    }

#undef READ_FLAT
#undef READ_LEGS

    return std::move(base->trade);
}

} // namespace ores::qt
