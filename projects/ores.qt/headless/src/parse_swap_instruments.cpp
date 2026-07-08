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

// This translation unit is intentionally isolated from parse_trade_instrument.cpp.
//
// MSVC C1202 ("recursive type or function dependency context too complex") fires
// when rfl::json::read<> is instantiated for too many types in the same TU: the
// accumulated rfl::StringLiteral<N> field-name types fill MSVC's internal template
// dependency graph.  Splitting all 9 swap-instrument types into this dedicated TU
// gives MSVC a clean slate; the flat/FX/equity types in parse_trade_instrument.cpp
// never enter this compilation context.

#include "ores.logging/make_logger.hpp"
#include "ores.qt.headless/IInstrumentFormPopulator.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "parse_swap_impl.hpp"
#include <optional>
#include <rfl/json.hpp>
#include <vector>

namespace {

using namespace ores::logging;
namespace td = ores::trading::domain;

inline std::string_view logger_name = "ores.qt.parse_swap_instruments";
[[nodiscard]] static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

template <typename T>
static std::optional<T> try_parse(const std::string& raw) {
    auto r = rfl::json::read<T>(raw);
    if (!r) {
        BOOST_LOG_SEV(lg(), error)
            << "parse_swap_instrument: deserialise failed: " << r.error().what();
        return std::nullopt;
    }
    return std::move(*r);
}

// Shared legs wrapper: swap_leg is the same for all 9 swap types.
struct swap_legs_inner {
    std::vector<td::swap_leg> legs;
};
struct swap_legs_outer {
    swap_legs_inner instrument;
};

// Per-type instrument wrappers: read response["instrument"]["instrument"].
struct fra_instr_inner {
    td::fra_instrument instrument;
};
struct fra_instr_outer {
    fra_instr_inner instrument;
};

struct vanilla_swap_instr_inner {
    td::vanilla_swap_instrument instrument;
};
struct vanilla_swap_instr_outer {
    vanilla_swap_instr_inner instrument;
};

struct cap_floor_instr_inner {
    td::cap_floor_instrument instrument;
};
struct cap_floor_instr_outer {
    cap_floor_instr_inner instrument;
};

struct swaption_instr_inner {
    td::swaption_instrument instrument;
};
struct swaption_instr_outer {
    swaption_instr_inner instrument;
};

struct bgs_instr_inner {
    td::balance_guaranteed_swap_instrument instrument;
};
struct bgs_instr_outer {
    bgs_instr_inner instrument;
};

struct callable_swap_instr_inner {
    td::callable_swap_instrument instrument;
};
struct callable_swap_instr_outer {
    callable_swap_instr_inner instrument;
};

struct knock_out_swap_instr_inner {
    td::knock_out_swap_instrument instrument;
};
struct knock_out_swap_instr_outer {
    knock_out_swap_instr_inner instrument;
};

struct inflation_swap_instr_inner {
    td::inflation_swap_instrument instrument;
};
struct inflation_swap_instr_outer {
    inflation_swap_instr_inner instrument;
};

struct rpa_instr_inner {
    td::rpa_instrument instrument;
};
struct rpa_instr_outer {
    rpa_instr_inner instrument;
};

template <typename InstrOuter, typename LegsOuter>
static bool try_parse_and_populate(const std::string& raw,
                                   ores::qt::IInstrumentFormPopulator& populator) {
    auto r_instr = try_parse<InstrOuter>(raw);
    if (!r_instr)
        return false;
    auto r_legs = try_parse<LegsOuter>(raw);
    if (!r_legs)
        return false;
    populator.populate(r_instr->instrument.instrument, std::move(r_legs->instrument.legs));
    return true;
}

} // namespace

namespace ores::qt::internal {

bool parse_swap_instrument(const std::string& raw,
                           const std::string& ttc,
                           ores::qt::IInstrumentFormPopulator& pop) {
    using namespace ores::logging;

    if (ttc == "ForwardRateAgreement") {
        BOOST_LOG_SEV(lg(), debug) << "parse_swap_instrument: reading fra_instrument";
        return try_parse_and_populate<fra_instr_outer, swap_legs_outer>(raw, pop);
    } else if (ttc == "Swap" || ttc == "CrossCurrencySwap" || ttc == "FlexiSwap") {
        BOOST_LOG_SEV(lg(), debug) << "parse_swap_instrument: reading vanilla_swap_instrument";
        return try_parse_and_populate<vanilla_swap_instr_outer, swap_legs_outer>(raw, pop);
    } else if (ttc == "CapFloor") {
        BOOST_LOG_SEV(lg(), debug) << "parse_swap_instrument: reading cap_floor_instrument";
        return try_parse_and_populate<cap_floor_instr_outer, swap_legs_outer>(raw, pop);
    } else if (ttc == "Swaption") {
        BOOST_LOG_SEV(lg(), debug) << "parse_swap_instrument: reading swaption_instrument";
        return try_parse_and_populate<swaption_instr_outer, swap_legs_outer>(raw, pop);
    } else if (ttc == "BalanceGuaranteedSwap") {
        BOOST_LOG_SEV(lg(), debug)
            << "parse_swap_instrument: reading balance_guaranteed_swap_instrument";
        return try_parse_and_populate<bgs_instr_outer, swap_legs_outer>(raw, pop);
    } else if (ttc == "CallableSwap") {
        BOOST_LOG_SEV(lg(), debug) << "parse_swap_instrument: reading callable_swap_instrument";
        return try_parse_and_populate<callable_swap_instr_outer, swap_legs_outer>(raw, pop);
    } else if (ttc == "KnockOutSwap") {
        BOOST_LOG_SEV(lg(), debug) << "parse_swap_instrument: reading knock_out_swap_instrument";
        return try_parse_and_populate<knock_out_swap_instr_outer, swap_legs_outer>(raw, pop);
    } else if (ttc == "InflationSwap") {
        BOOST_LOG_SEV(lg(), debug) << "parse_swap_instrument: reading inflation_swap_instrument";
        return try_parse_and_populate<inflation_swap_instr_outer, swap_legs_outer>(raw, pop);
    } else if (ttc == "RiskParticipationAgreement") {
        BOOST_LOG_SEV(lg(), debug) << "parse_swap_instrument: reading rpa_instrument";
        return try_parse_and_populate<rpa_instr_outer, swap_legs_outer>(raw, pop);
    }

    BOOST_LOG_SEV(lg(), warn) << "parse_swap_instrument: unknown trade_type: " << ttc;
    return false;
}

} // namespace ores::qt::internal
