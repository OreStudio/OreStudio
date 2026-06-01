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
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
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

// Attempt to parse the raw response JSON as T. Returns nullopt and logs on failure.
template<typename T>
static std::optional<T> try_parse(const std::string& raw) {
    auto r = rfl::json::read<T>(raw);
    if (!r) {
        BOOST_LOG_SEV(lg(), error)
            << "getTradeInstrument: deserialise failed: " << r.error().what();
        return std::nullopt;
    }
    return std::move(*r);
}

// Phase 1: envelope — rfl silently skips the "instrument" JSON key.
struct response_envelope {
    bool success = false;
    std::string message;
    td::trade trade;
};

// Phase 2: one named wrapper per concrete leaf type.
//
// Flat types (bond, credit, commodity, scripted, fx_*, equity_*):
//   response["instrument"] = {concrete_instrument_fields}
//
// with_legs types (composite, swap):
//   response["instrument"] = {"instrument": {concrete_fields}, "legs": [...]}
//
// MSVC C1202 note: reflecting a combined {concrete_instrument; vector<*_leg>} struct in
// a single rfl::json::read call creates a Literal over the union of all unique field
// names from both types (~24-29 combined). This exceeds MSVC's recursive template
// dependency limit. Fix: split into two reads — one for "instrument.instrument",
// one for "instrument.legs". Each read sees at most ~19 fields, well within the limit.

// --- Flat types ---

struct bond_wrapper      { td::bond_instrument      instrument; };
struct credit_wrapper    { td::credit_instrument    instrument; };
struct commodity_wrapper { td::commodity_instrument instrument; };
struct scripted_wrapper  { td::scripted_instrument  instrument; };

// --- Composite (composite_instrument + composite legs): split reads ---

struct composite_instr_inner { td::composite_instrument            instrument; };
struct composite_instr_outer { composite_instr_inner               instrument; };

struct composite_legs_inner  { std::vector<td::composite_leg>      legs; };
struct composite_legs_outer  { composite_legs_inner                instrument; };

// --- Rates/swap types (swap_leg shared): split reads ---
//
// Instrument-only wrappers: read response["instrument"]["instrument"].

struct fra_instr_inner          { td::fra_instrument                    instrument; };
struct fra_instr_outer          { fra_instr_inner                       instrument; };

struct vanilla_swap_instr_inner { td::vanilla_swap_instrument           instrument; };
struct vanilla_swap_instr_outer { vanilla_swap_instr_inner              instrument; };

struct cap_floor_instr_inner    { td::cap_floor_instrument              instrument; };
struct cap_floor_instr_outer    { cap_floor_instr_inner                 instrument; };

struct swaption_instr_inner     { td::swaption_instrument               instrument; };
struct swaption_instr_outer     { swaption_instr_inner                  instrument; };

struct bgs_instr_inner          { td::balance_guaranteed_swap_instrument instrument; };
struct bgs_instr_outer          { bgs_instr_inner                       instrument; };

struct callable_swap_instr_inner { td::callable_swap_instrument         instrument; };
struct callable_swap_instr_outer { callable_swap_instr_inner            instrument; };

struct knock_out_swap_instr_inner { td::knock_out_swap_instrument       instrument; };
struct knock_out_swap_instr_outer { knock_out_swap_instr_inner          instrument; };

struct inflation_swap_instr_inner { td::inflation_swap_instrument       instrument; };
struct inflation_swap_instr_outer { inflation_swap_instr_inner          instrument; };

struct rpa_instr_inner          { td::rpa_instrument                    instrument; };
struct rpa_instr_outer          { rpa_instr_inner                       instrument; };

// Legs-only wrapper: shared across all swap types.
struct swap_legs_inner { std::vector<td::swap_leg> legs; };
struct swap_legs_outer { swap_legs_inner instrument; };

// --- FX types ---

struct fx_forward_wrapper        { td::fx_forward_instrument        instrument; };
struct fx_vanilla_option_wrapper { td::fx_vanilla_option_instrument instrument; };
struct fx_barrier_option_wrapper { td::fx_barrier_option_instrument instrument; };
struct fx_digital_option_wrapper { td::fx_digital_option_instrument instrument; };
struct fx_asian_forward_wrapper  { td::fx_asian_forward_instrument  instrument; };
struct fx_accumulator_wrapper    { td::fx_accumulator_instrument    instrument; };
struct fx_variance_swap_wrapper  { td::fx_variance_swap_instrument  instrument; };

// --- Equity types ---

struct eq_option_wrapper    { td::equity_option_instrument          instrument; };
struct eq_forward_wrapper   { td::equity_forward_instrument         instrument; };
struct eq_swap_wrapper      { td::equity_swap_instrument            instrument; };
struct eq_var_swap_wrapper  { td::equity_variance_swap_instrument   instrument; };
struct eq_barrier_wrapper   { td::equity_barrier_option_instrument  instrument; };
struct eq_asian_wrapper     { td::equity_asian_option_instrument    instrument; };
struct eq_digital_wrapper   { td::equity_digital_option_instrument  instrument; };
struct eq_accum_wrapper     { td::equity_accumulator_instrument     instrument; };
struct eq_position_wrapper  { td::equity_position_instrument        instrument; };

// Two-pass helper for leg-based types: parse instrument, short-circuit on failure,
// then parse legs, then call the matching populate overload.
template<typename InstrOuter, typename LegsOuter>
static bool try_parse_and_populate(const std::string& raw,
    ores::qt::IInstrumentFormPopulator& populator)
{
    auto r_instr = try_parse<InstrOuter>(raw);
    if (!r_instr) return false;
    auto r_legs = try_parse<LegsOuter>(raw);
    if (!r_legs) return false;
    populator.populate(r_instr->instrument.instrument,
                       std::move(r_legs->instrument.legs));
    return true;
}

} // namespace

namespace ores::qt {

std::optional<trading::domain::trade>
parse_trade_instrument(const std::string& raw, IInstrumentFormPopulator& populator) {
    // Phase 1: parse envelope — no variant, no AddTagsToVariants.
    BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: phase 1 parse";
    auto base = try_parse<response_envelope>(raw);
    if (!base) return std::nullopt;
    if (!base->success) {
        BOOST_LOG_SEV(lg(), error)
            << "getTradeInstrument: server error: " << base->message;
        return std::nullopt;
    }

    const auto& cls = base->trade.classification;
    const auto  pt  = cls.product_type;
    const auto& ttc = cls.trade_type;

    BOOST_LOG_SEV(lg(), debug)
        << "getTradeInstrument: product_type=" << td::to_string(pt)
        << " trade_type=" << ttc;

    // Phase 2: dispatch on (product_type, trade_type) — read concrete leaf
    // struct directly, no AddTagsToVariants, no trade_instrument variant.
    using ores::trading::domain::product_type;

    switch (pt) {
    case product_type::bond: {
        BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading bond_instrument";
        auto r = try_parse<bond_wrapper>(raw);
        if (!r) return std::nullopt;
        populator.populate(r->instrument);
        break;
    }
    case product_type::credit: {
        BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading credit_instrument";
        auto r = try_parse<credit_wrapper>(raw);
        if (!r) return std::nullopt;
        populator.populate(r->instrument);
        break;
    }
    case product_type::commodity: {
        BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading commodity_instrument";
        auto r = try_parse<commodity_wrapper>(raw);
        if (!r) return std::nullopt;
        populator.populate(r->instrument);
        break;
    }
    case product_type::scripted: {
        BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading scripted_instrument";
        auto r = try_parse<scripted_wrapper>(raw);
        if (!r) return std::nullopt;
        populator.populate(r->instrument);
        break;
    }
    case product_type::composite: {
        BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading composite_instrument";
        if (!try_parse_and_populate<composite_instr_outer, composite_legs_outer>(raw, populator))
            return std::nullopt;
        break;
    }
    case product_type::swap: {
        if (ttc == "ForwardRateAgreement") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fra_instrument";
            if (!try_parse_and_populate<fra_instr_outer, swap_legs_outer>(raw, populator))
                return std::nullopt;
        } else if (ttc == "Swap" || ttc == "CrossCurrencySwap" || ttc == "FlexiSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading vanilla_swap_instrument";
            if (!try_parse_and_populate<vanilla_swap_instr_outer, swap_legs_outer>(raw, populator))
                return std::nullopt;
        } else if (ttc == "CapFloor") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading cap_floor_instrument";
            if (!try_parse_and_populate<cap_floor_instr_outer, swap_legs_outer>(raw, populator))
                return std::nullopt;
        } else if (ttc == "Swaption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading swaption_instrument";
            if (!try_parse_and_populate<swaption_instr_outer, swap_legs_outer>(raw, populator))
                return std::nullopt;
        } else if (ttc == "BalanceGuaranteedSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading balance_guaranteed_swap_instrument";
            if (!try_parse_and_populate<bgs_instr_outer, swap_legs_outer>(raw, populator))
                return std::nullopt;
        } else if (ttc == "CallableSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading callable_swap_instrument";
            if (!try_parse_and_populate<callable_swap_instr_outer, swap_legs_outer>(raw, populator))
                return std::nullopt;
        } else if (ttc == "KnockOutSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading knock_out_swap_instrument";
            if (!try_parse_and_populate<knock_out_swap_instr_outer, swap_legs_outer>(raw, populator))
                return std::nullopt;
        } else if (ttc == "InflationSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading inflation_swap_instrument";
            if (!try_parse_and_populate<inflation_swap_instr_outer, swap_legs_outer>(raw, populator))
                return std::nullopt;
        } else if (ttc == "RiskParticipationAgreement") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading rpa_instrument";
            if (!try_parse_and_populate<rpa_instr_outer, swap_legs_outer>(raw, populator))
                return std::nullopt;
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "getTradeInstrument: unknown (product_type, trade_type) — ("
                << td::to_string(pt) << ", " << ttc << "); returning monostate";
            return std::nullopt;
        }
        break;
    }
    case product_type::fx: {
        if (ttc == "FxForward" || ttc == "FxSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_forward_instrument";
            auto r = try_parse<fx_forward_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "FxOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_vanilla_option_instrument";
            auto r = try_parse<fx_vanilla_option_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "FxBarrierOption" || ttc == "FxGenericBarrierOption"
                   || ttc == "FxDoubleBarrierOption" || ttc == "FxEuropeanBarrierOption"
                   || ttc == "FxKIKOBarrierOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_barrier_option_instrument";
            auto r = try_parse<fx_barrier_option_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "FxDigitalOption" || ttc == "FxDigitalBarrierOption"
                   || ttc == "FxTouchOption" || ttc == "FxDoubleTouchOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_digital_option_instrument";
            auto r = try_parse<fx_digital_option_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "FxAverageForward" || ttc == "FxTaRF") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_asian_forward_instrument";
            auto r = try_parse<fx_asian_forward_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "FxAccumulator") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_accumulator_instrument";
            auto r = try_parse<fx_accumulator_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "FxVarianceSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_variance_swap_instrument";
            auto r = try_parse<fx_variance_swap_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "getTradeInstrument: unknown (product_type, trade_type) — ("
                << td::to_string(pt) << ", " << ttc << "); returning monostate";
            return std::nullopt;
        }
        break;
    }
    case product_type::equity: {
        if (ttc == "EquityOption" || ttc == "EquityCliquetOption"
            || ttc == "EquityOutperformanceOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_option_instrument";
            auto r = try_parse<eq_option_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "EquityForward") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_forward_instrument";
            auto r = try_parse<eq_forward_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "EquitySwap" || ttc == "EquityWorstOfBasketSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_swap_instrument";
            auto r = try_parse<eq_swap_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "EquityVarianceSwap") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_variance_swap_instrument";
            auto r = try_parse<eq_var_swap_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "EquityBarrierOption" || ttc == "EquityDoubleBarrierOption"
                   || ttc == "EquityEuropeanBarrierOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_barrier_option_instrument";
            auto r = try_parse<eq_barrier_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "EquityAsianOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_asian_option_instrument";
            auto r = try_parse<eq_asian_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "EquityDigitalOption" || ttc == "EquityTouchOption") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_digital_option_instrument";
            auto r = try_parse<eq_digital_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "EquityAccumulator" || ttc == "EquityTaRF") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_accumulator_instrument";
            auto r = try_parse<eq_accum_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else if (ttc == "EquityPosition") {
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_position_instrument";
            auto r = try_parse<eq_position_wrapper>(raw);
            if (!r) return std::nullopt;
            populator.populate(r->instrument);
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "getTradeInstrument: unknown (product_type, trade_type) — ("
                << td::to_string(pt) << ", " << ttc << "); returning monostate";
            return std::nullopt;
        }
        break;
    }
    case product_type::unknown:
    default:
        BOOST_LOG_SEV(lg(), warn)
            << "getTradeInstrument: unknown (product_type, trade_type) — ("
            << td::to_string(pt) << ", " << ttc << "); returning monostate";
        return std::nullopt;
    }

    return std::move(base->trade);
}

} // namespace ores::qt
