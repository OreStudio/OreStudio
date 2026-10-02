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
#ifndef ORES_MARKETDATA_API_DATUM_ORE_TYPES_HPP
#define ORES_MARKETDATA_API_DATUM_ORE_TYPES_HPP

#include "ores.marketdata.api/export.hpp"
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string_view>

namespace ores::marketdata::datum {

/**
 * @brief ORE's market datum instrument types: MarketDatum::InstrumentType in
 * OREData, without NONE.
 *
 * The order is ORE's, so the enum and ORE's own list can be compared line by
 * line.
 */
enum class instrument_type : std::uint8_t {
    zero,
    discount,
    mm,
    mm_future,
    oi_future,
    fra,
    imm_fra,
    ir_swap,
    basis_swap,
    bma_swap,
    cc_basis_swap,
    cc_fix_float_swap,
    cds,
    cds_index,
    fx_spot,
    fx_fwd,
    hazard_rate,
    recovery_rate,
    assumed_recovery_rate,
    swaption,
    capfloor,
    fx_option,
    zc_inflation_swap,
    zc_inflation_capfloor,
    yy_inflation_swap,
    yy_inflation_capfloor,
    seasonality,
    equity_spot,
    equity_fwd,
    equity_dividend,
    equity_option,
    bond,
    bond_future,
    bond_option,
    bond_future_option,
    index_cds_option,
    index_cds_tranche,
    commodity_spot,
    commodity_fwd,
    correlation,
    commodity_option,
    commodity_calendar_spread_option,
    shape_profile,
    cpr,
    rating
};

inline constexpr std::size_t instrument_type_count = 45;

/**
 * @brief ORE's market datum quote types: MarketDatum::QuoteType in OREData,
 * without NONE.
 */
enum class quote_type : std::uint8_t {
    basis_spread,
    credit_spread,
    conv_credit_spread,
    yield_spread,
    hazard_rate,
    rate,
    ratio,
    price,
    rate_lnvol,
    rate_nvol,
    rate_slnvol,
    base_correlation,
    shift,
    transition_probability,
    conversion_factor,
    shape_factor
};

inline constexpr std::size_t quote_type_count = 16;

/**
 * @brief The name ORE's enum gives the type, such as FX_SPOT or
 * ZC_INFLATIONSWAP.
 *
 * Not always the key token: a key spells FX_SPOT as FX and EQUITY_SPOT as
 * EQUITY. The key grammar owns the tokens.
 */
ORES_MARKETDATA_API_EXPORT std::string_view ore_name(instrument_type t);

/// The name ORE's enum gives the quote type, such as RATE_LNVOL.
ORES_MARKETDATA_API_EXPORT std::string_view ore_name(quote_type t);

/// The instrument type ORE's enum names @p name, or nothing.
ORES_MARKETDATA_API_EXPORT std::optional<instrument_type>
instrument_type_named(std::string_view name);

/// The quote type ORE's enum names @p name, or nothing.
ORES_MARKETDATA_API_EXPORT std::optional<quote_type> quote_type_named(std::string_view name);

}

#endif
