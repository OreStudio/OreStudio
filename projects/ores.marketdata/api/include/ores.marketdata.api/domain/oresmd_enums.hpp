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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: oresmd_enums.hpp.mustache
 * To modify, update the template and regenerate.
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
    rate,                  ///< A rate quote (e.g. MM/RATE, FRA/RATE, IR_SWAP/RATE, ZERO/RATE).
    price,                 ///< A price quote (e.g. MM_FUTURE/PRICE, OI_FUTURE/PRICE).
    basis_spread,          ///< A basis spread quote (e.g. BASIS_SWAP/BASIS_SPREAD).
    ratio,                 ///< A ratio quote (e.g. BMA_SWAP/RATIO).
    yield_spread,          ///< A yield spread quote (e.g. ZERO/YIELD_SPREAD).
    conversion_factor,     ///< A conversion factor quote (e.g. BOND/CONVERSION_FACTOR).
    shape_factor,          ///< A shape factor quote (e.g. SHAPE_PROFILE/SHAPE_FACTOR).
    transition_probability ///< A rating transition probability (e.g.
                           ///< RATING/TRANSITION_PROBABILITY).
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
    oi_future,         ///< OI_FUTURE (overnight index future price).
    capfloor,          ///< CAPFLOOR (cap/floor volatility on a strike grid).
    bond_option        ///< BOND_OPTION (bond option implied vol).
};

/**
 * @brief The `index` query key of an oresmd URI, IR-only: a fixed benchmark-family token,
 * not free text -- closes the gap-analysis's "index_name is free text" finding.
 *
 * The values mirror the family list the CHECK constraint on
 * ores_synthetic_ir_curve_generation_configs_tbl
 * (synthetic_ir_curve_generation_configs_create.sql) admits, and the two move
 * together: a family this enum holds and that constraint does not is a family no
 * synthetic curve can be configured for.
 */
enum class index_family {
    libor,
    euribor,
    sofr,
    estr,
    sonia,
    tona,
    saron,
    aonia,
    corra,
    honia,
    sora,
    swestr,
    nowa,
    kofr,
    mibor,
    zaronia,
    destr,
    polonia,
    nzonia,
    shibor,
    tiie,
    ftiie,
    taibor,
    bbsw,
    cibor,
    cms,
    hibor,
    nibor,
    pribor,
    repofix,
    stibor,
    camara,
    czeonia,
    dkkois,
    eonia,
    fedfunds,
    sifma,
    sior
};

/**
 * @brief Whether every index name ORE writes for this family carries a tenor --
 * USD-LIBOR-3M, where no USD-LIBOR exists -- rather than the family also being
 * written bare, as USD-SOFR is.
 *
 * False does not forbid a tenor. The corpus writes both USD-SOFR and USD-SOFR-3M,
 * as two different fixings, so a family this answers false for is written both
 * ways and the identifier's own fields decide which name is emitted.
 *
 * Generated from the Index family table of ir_quote_type.org, which is where the
 * rule is written and where the reason for each family's answer sits beside it.
 * The switch is exhaustive and has no default, so a family added to the enum
 * without being classified fails to compile: that is the only thing keeping the
 * two from drifting apart.
 */
inline bool requires_tenor(index_family f) {
    switch (f) {
        case index_family::libor:
            return true;
        case index_family::euribor:
            return true;
        case index_family::sofr:
            return false;
        case index_family::estr:
            return false;
        case index_family::sonia:
            return false;
        case index_family::tona:
            return false;
        case index_family::saron:
            return false;
        case index_family::aonia:
            return false;
        case index_family::corra:
            return false;
        case index_family::honia:
            return false;
        case index_family::sora:
            return false;
        case index_family::swestr:
            return false;
        case index_family::nowa:
            return false;
        case index_family::kofr:
            return false;
        case index_family::mibor:
            return false;
        case index_family::zaronia:
            return false;
        case index_family::destr:
            return false;
        case index_family::polonia:
            return false;
        case index_family::nzonia:
            return false;
        case index_family::shibor:
            return true;
        case index_family::tiie:
            return true;
        case index_family::ftiie:
            return false;
        case index_family::taibor:
            return true;
        case index_family::bbsw:
            return true;
        case index_family::cibor:
            return true;
        case index_family::cms:
            return true;
        case index_family::hibor:
            return true;
        case index_family::nibor:
            return true;
        case index_family::pribor:
            return true;
        case index_family::repofix:
            return true;
        case index_family::stibor:
            return true;
        case index_family::camara:
            return false;
        case index_family::czeonia:
            return false;
        case index_family::dkkois:
            return false;
        case index_family::eonia:
            return false;
        case index_family::fedfunds:
            return false;
        case index_family::sifma:
            return false;
        case index_family::sior:
            return false;
    }
    return false;
}

/**
 * @brief The `quote` query key for credit instruments — the ORE TYPE, independent of the
 * METRIC column. Credit-only; only meaningful when `type=quote`.
 */
enum class credit_quote_type {
    cds, ///< CDS/CREDIT_SPREAD/ENTITY/SENIORITY/CCY/TENOR, and the seven-segment form carrying the
         ///< restructuring clause between the currency and the tenor (XR14, MR14)
    hazard_rate,   ///< HAZARD_RATE/RATE/ENTITY/SENIORITY/CCY/TENOR, with the same optional
                   ///< restructuring clause
    recovery_rate, ///< RECOVERY_RATE/RATE/ENTITY/SENIORITY/CCY, plus the restructuring clause on
                   ///< the keys that name one
    cds_index,     ///< CDS_INDEX/BASE_CORRELATION (index base correlation).
    index_cds_tranche, ///< INDEX_CDS_TRANCHE/BASE_CORRELATION (tranche base correlation).
    index_cds_option   ///< INDEX_CDS_OPTION/MODEL/INDEX/TENOR/EXPIRY/STRIKE, plus the four-segment
                       ///< term-vol form INDEX_CDS_OPTION/MODEL/INDEX/TENOR; the metric segment is
                       ///< the vol model, not this table's ore_metric
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

/**
 * @brief The `quote` query key for commodity instruments — the ORE TYPE. Commodity-only;
 * only meaningful when `type=quote`.
 */
enum class commodity_quote_type {
    spot,  ///< COMMODITY/PRICE (spot price, the default).
    fwd,   ///< COMMODITY_FWD/PRICE (commodity forward price).
    option ///< COMMODITY_OPTION/MODEL/CODE/CCY/EXPIRY[/DELTA/PREMIUM/CALL_PUT]/STRIKE -- the equity
           ///< option's three shapes, with a commodity code where the equity has a ticker; the
           ///< metric segment is the vol model, not this table's ore_metric
};

/**
 * @brief The `quote` query key for FX instruments — the ORE TYPE. FX-only;
 * only meaningful when `type=quote`.
 */
enum class fx_quote_type {
    spot, ///< FX/RATE (spot rate, the default).
    fwd   ///< FXFWD/RATE (forward points).
};

/**
 * @brief The `quote` query key for inflation instruments — the ORE TYPE. Inflation-only;
 * only meaningful when `type=quote`.
 */
enum class inflation_quote_type {
    zc_swap,     ///< ZC_INFLATIONSWAP/RATE (zero-coupon inflation swap rate).
    yy_swap,     ///< YY_INFLATIONSWAP/RATE (year-on-year inflation swap rate).
    seasonality, ///< SEASONALITY/RATE (seasonality adjustment factor).
    zc_capfloor, ///< 6-segment: ZC_INFLATIONCAPFLOOR/PRICE/INDEX/MATURITY/CAP_OR_FLOOR/STRIKE, and
                 ///< the same shape under RATE_NVOL.
    yy_capfloor, ///< 6-segment: YY_INFLATIONCAPFLOOR/PRICE/INDEX/MATURITY/CAP_OR_FLOOR/STRIKE, and
                 ///< the same shape under RATE_NVOL.
    cf_price     ///< 6-segment: CAPFLOOR/PRICE/INDEX/MATURITY/CAP_OR_FLOOR/STRIKE -- the inflation
                 ///< cap/floor price under the older CAPFLOOR type name, which the corpus carries
                 ///< beside ZC_INFLATIONCAPFLOOR/PRICE for the same instrument.
};

/**
 * @brief The `quote` query key for correlation instruments — the ORE TYPE. Correlation-only;
 * only meaningful when `type=quote`.
 */
enum class correlation_quote_type {
    pairwise ///< CORRELATION/RATE (pairwise factor correlation): one factor pair alone, or two
             ///< operands with the expiry and strike after them.
};

/**
 * @brief The `quote` query key for security instruments. Security-only; only meaningful
 * when `type=quote`.
 */
enum class security_quote_type {
    bond_price,             ///< BOND/PRICE (bond clean price).
    bond_yield_spread,      ///< BOND/YIELD_SPREAD (bond yield spread).
    bond_conversion_factor, ///< BOND/CONVERSION_FACTOR (bond futures conversion factor).
    recovery_rate, ///< RECOVERY_RATE/RATE (recovery assumption named by a security rather than by
                   ///< an entity and a seniority).
    cpr ///< CPR/RATE (conditional prepayment rate). The corpus keys it by ISIN, inside `<Security>`
        ///< blocks in curveconfig.xml, so it belongs to this class rather than to commodity, which
        ///< carried it as a simplification while no security-level identifier existed.
};

/**
 * @brief The `quote` query key for shape profiles. Shape-profile-only; only meaningful
 * when `type=quote`.
 */
enum class shape_profile_quote_type {
    shape_factor ///< SHAPE_PROFILE/SHAPE_FACTOR/PROFILE/DATE/SECOND/PERIOD, plus the DST flag the
                 ///< corpus writes as a seventh segment.
};

/**
 * @brief The `quote` query key for rating providers. Rating-only; only meaningful when
 * `type=quote`.
 */
enum class rating_quote_type {
    transition_probability ///< RATING/TRANSITION_PROBABILITY/PROVIDER/FROM/TO, and the four-segment
                           ///< form the corpus also writes, which names the provider and no grades.
};

/**
 * @brief Volatility model subtype — the third segment of ORE's vol quote key
 * (e.g. RATE_LNVOL in SWAPTION/RATE_LNVOL/EUR/5Y/2Y/ATM). Shared across all
 * volatility sub-families per the vol sub-schema design.
 */
enum class volatility_model_subtype {
    rate_lnvol,  ///< RATE_LNVOL (log-normal volatility, the default).
    rate_nvol,   ///< RATE_NVOL (normal volatility).
    rate_slnvol, ///< RATE_SLNVOL (shifted log-normal volatility).
    shift,       ///< SHIFT (shift surface).
    price        ///< PRICE (price surface).
};

}

#endif
