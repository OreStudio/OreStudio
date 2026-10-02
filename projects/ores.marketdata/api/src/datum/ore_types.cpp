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
#include "ores.marketdata.api/datum/ore_types.hpp"
#include <array>

namespace ores::marketdata::datum {

namespace {

constexpr std::array<std::string_view, instrument_type_count> instrument_type_names{
    "ZERO",
    "DISCOUNT",
    "MM",
    "MM_FUTURE",
    "OI_FUTURE",
    "FRA",
    "IMM_FRA",
    "IR_SWAP",
    "BASIS_SWAP",
    "BMA_SWAP",
    "CC_BASIS_SWAP",
    "CC_FIX_FLOAT_SWAP",
    "CDS",
    "CDS_INDEX",
    "FX_SPOT",
    "FX_FWD",
    "HAZARD_RATE",
    "RECOVERY_RATE",
    "ASSUMED_RECOVERY_RATE",
    "SWAPTION",
    "CAPFLOOR",
    "FX_OPTION",
    "ZC_INFLATIONSWAP",
    "ZC_INFLATIONCAPFLOOR",
    "YY_INFLATIONSWAP",
    "YY_INFLATIONCAPFLOOR",
    "SEASONALITY",
    "EQUITY_SPOT",
    "EQUITY_FWD",
    "EQUITY_DIVIDEND",
    "EQUITY_OPTION",
    "BOND",
    "BOND_FUTURE",
    "BOND_OPTION",
    "BOND_FUTURE_OPTION",
    "INDEX_CDS_OPTION",
    "INDEX_CDS_TRANCHE",
    "COMMODITY_SPOT",
    "COMMODITY_FWD",
    "CORRELATION",
    "COMMODITY_OPTION",
    "COMMODITY_CALENDAR_SPREAD_OPTION",
    "SHAPE_PROFILE",
    "CPR",
    "RATING"};

constexpr std::array<std::string_view, quote_type_count> quote_type_names{"BASIS_SPREAD",
                                                                          "CREDIT_SPREAD",
                                                                          "CONV_CREDIT_SPREAD",
                                                                          "YIELD_SPREAD",
                                                                          "HAZARD_RATE",
                                                                          "RATE",
                                                                          "RATIO",
                                                                          "PRICE",
                                                                          "RATE_LNVOL",
                                                                          "RATE_NVOL",
                                                                          "RATE_SLNVOL",
                                                                          "BASE_CORRELATION",
                                                                          "SHIFT",
                                                                          "TRANSITION_PROBABILITY",
                                                                          "CONVERSION_FACTOR",
                                                                          "SHAPE_FACTOR"};

template <class E, std::size_t N>
std::optional<E> named(const std::array<std::string_view, N>& names, std::string_view name) {
    for (std::size_t i = 0; i < N; ++i) {
        if (names[i] == name)
            return static_cast<E>(i);
    }
    return std::nullopt;
}

}

std::string_view ore_name(instrument_type t) {
    return instrument_type_names[static_cast<std::size_t>(t)];
}

std::string_view ore_name(quote_type t) {
    return quote_type_names[static_cast<std::size_t>(t)];
}

std::optional<instrument_type> instrument_type_named(std::string_view name) {
    return named<instrument_type>(instrument_type_names, name);
}

std::optional<quote_type> quote_type_named(std::string_view name) {
    return named<quote_type>(quote_type_names, name);
}

}
