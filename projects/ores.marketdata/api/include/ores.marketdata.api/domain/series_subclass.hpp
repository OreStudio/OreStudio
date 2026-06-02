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
#ifndef ORES_MARKETDATA_API_DOMAIN_SERIES_SUBCLASS_HPP
#define ORES_MARKETDATA_API_DOMAIN_SERIES_SUBCLASS_HPP

namespace ores::marketdata::domain {

/**
 * @brief Fine-grained subclass of a market series within its asset class.
 *
 * Combined with asset_class, this covers all 35 ORE key types and supports
 * efficient slice queries (e.g. "all FX vol surfaces for a tenant").
 *
 * Mapping:
 *   FX:          spot (FX), forward (FXFWD), volatility (FX_OPTION)
 *   RATES:       yield (MM/DISCOUNT/ZERO/IR_SWAP), volatility (SWAPTION/CAPFLOOR),
 *                basis (BASIS_SWAP/BMA_SWAP), fra (FRA/IMM_FRA/MM_FUTURE),
 *                xccy (CC_BASIS_SWAP/CC_FIX_FLOAT_SWAP)
 *   CREDIT:      spread (HAZARD_RATE/CDS), index_credit (CDS_INDEX/INDEX_CDS_OPTION),
 *                recovery (RECOVERY_RATE)
 *   EQUITY:      spot (EQUITY), forward (EQUITY_FWD/EQUITY_DIVIDEND),
 *                volatility (EQUITY_OPTION)
 *   COMMODITY:   spot (COMMODITY), forward (COMMODITY_FWD),
 *                volatility (COMMODITY_OPTION)
 *   INFLATION:   swap (ZC_INFLATIONSWAP/YY_INFLATIONSWAP),
 *                capfloor (ZC_INFLATIONCAPFLOOR/YY_INFLATIONCAPFLOOR),
 *                seasonality (SEASONALITY)
 *   BOND:        price (BOND/PRICE), spread (BOND/YIELD_SPREAD)
 *   CROSS_ASSET: correlation (CORRELATION)
 */
enum class series_subclass {
    spot,          ///< Spot price or FX rate (scalar).
    forward,       ///< Forward curve.
    volatility,    ///< Volatility surface or curve.
    yield,         ///< Yield / discount / zero-rate curve.
    basis,         ///< Basis spread curve.
    fra,           ///< FRA or money-market futures curve.
    xccy,          ///< Cross-currency basis swap curve.
    spread,        ///< Credit spread or hazard rate curve.
    index_credit,  ///< CDS index or index option (avoids clash with std::index).
    recovery,      ///< Recovery rate (scalar per entity/seniority/ccy).
    swap,          ///< Inflation swap zero-coupon or year-on-year curve.
    capfloor,      ///< Cap/floor volatility surface.
    seasonality,   ///< Inflation seasonality adjustment factors.
    price,         ///< Bond price.
    correlation    ///< Cross-asset or cross-currency correlation.
};

}

#endif
