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
#ifndef ORES_MARKETDATA_API_DOMAIN_ORESMD_ENUMS_HPP
#define ORES_MARKETDATA_API_DOMAIN_ORESMD_ENUMS_HPP

namespace ores::marketdata::domain {

/**
 * @brief The `type` query key of an oresmd URI: what kind of thing the identifier names,
 * independent of its coordinates.
 *
 * See id:C3E053CA-0D4B-480B-9119-E11530160EC1 ("oresmd: ORE Studio Market Data URI"),
 * "Grammar" section.
 */
enum class instrument_type {
    fixing, ///< A rate fixing/index (projects to ORE's index name).
    curve,  ///< A whole curve (projects to ORE's Yield/<CCY>/<CURVE_ID> curve key).
    quote,  ///< A single published quote (projects to an ORE quote key).
    vol     ///< A volatility surface point (projects to an ORE quote key).
};

/**
 * @brief The `role` query key of an oresmd URI, IR-only: whether a curve discounts or
 * projects, closing the gap-analysis's discount-vs-projection ambiguity.
 */
enum class curve_role {
    discount,        ///< This curve discounts cashflows.
    projection,      ///< This curve projects a floating index's forward rate.
    self_discounting ///< Degenerate case: one curve does both (current synthetic data's shape).
};

/**
 * @brief The `metric` query key of an oresmd URI, only meaningful when `type=quote`:
 * the ORE METRIC column. Defaults from the `quote` type when absent (e.g. quote=mm
 * implies metric=rate, quote=mm_future implies metric=price).
 */
enum class metric {
    rate,         ///< A rate quote (e.g. MM/RATE, FRA/RATE, IR_SWAP/RATE, ZERO/RATE).
    price,        ///< A price quote (e.g. MM_FUTURE/PRICE, OI_FUTURE/PRICE).
    basis_spread, ///< A basis spread quote (e.g. BASIS_SWAP/BASIS_SPREAD).
    ratio,        ///< A ratio quote (e.g. BMA_SWAP/RATIO).
    yield_spread  ///< A yield spread quote (e.g. ZERO/YIELD_SPREAD).
};

/**
 * @brief The `quote` query key of an oresmd URI, IR-only and only meaningful when
 * `type=quote`: the ORE quote TYPE (e.g. MM, FRA, IR_SWAP) — the first segment of ORE's
 * TYPE/METRIC/... quote key, independent of the METRIC column carried by the `metric`
 * query key.
 */
enum class ir_quote_type {
    ir_swap,           ///< IR_SWAP (par swap rate).
    discount,          ///< DISCOUNT (curve-sampled discount factor).
    mm,                ///< MM (money market rate).
    fra,               ///< FRA (forward rate agreement rate).
    imm_fra,           ///< IMM_FRA (IMM-settled FRA rate).
    basis_swap,        ///< BASIS_SWAP (single-currency basis swap spread).
    bma_swap,          ///< BMA_SWAP (Bond Market Association swap ratio).
    cc_basis_swap,     ///< CC_BASIS_SWAP (cross-currency basis swap spread).
    cc_fix_float_swap, ///< CC_FIX_FLOAT_SWAP (cross-currency fix-float swap rate).
    zero,              ///< ZERO (zero-coupon rate).
    mm_future,         ///< MM_FUTURE (money market future price).
    oi_future          ///< OI_FUTURE (overnight index future price).
};

/**
 * @brief The `index` query key of an oresmd URI, IR-only: a fixed benchmark-family token,
 * not free text -- closes the gap-analysis's "index_name is free text" finding.
 */
enum class index_family { libor, euribor, sofr, estr, sonia, tona };

/**
 * @brief The `quote` query key for credit instruments — the ORE TYPE, independent of the
 * METRIC column. Credit-only; only meaningful when `type=quote`.
 */
enum class credit_quote_type {
    cds,              ///< CDS/CREDIT_SPREAD (single-name CDS spread).
    hazard_rate,      ///< HAZARD_RATE/RATE (bootstrapped hazard rate).
    recovery_rate,    ///< RECOVERY_RATE/RATE (recovery rate assumption).
    cds_index,        ///< CDS_INDEX/BASE_CORRELATION (index base correlation).
    index_cds_tranche ///< INDEX_CDS_TRANCHE/BASE_CORRELATION (tranche base correlation).
    // rating descoped — RATING/TRANSITION_PROBABILITY needs provider/from_rating/to_rating
    // fields the current credit_market_data_identifier has no equivalent for; tracked for
    // its own task.
};

/**
 * @brief The `quote` query key for equity instruments — the ORE TYPE. Equity-only;
 * only meaningful when `type=quote`.
 */
enum class equity_quote_type {
    spot,     ///< EQUITY/PRICE (spot price, the default).
    dividend, ///< EQUITY_DIVIDEND/RATE (dividend yield rate).
    fwd       ///< EQUITY_FWD/PRICE (equity forward price).
};

}

#endif
