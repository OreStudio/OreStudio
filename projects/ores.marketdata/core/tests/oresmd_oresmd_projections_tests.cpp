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
#include "ores.marketdata.core/oresmd/oresmd_exception.hpp"
#include "ores.marketdata.core/oresmd/oresmd_parser.hpp"
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include "ores.ore.core/market/fx_quote_convention_checker.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

using namespace ores::marketdata::domain;
using ores::marketdata::core::oresmd_parser;
using ores::marketdata::core::oresmd_projections;

const std::string tags("[oresmd][projections]");

ores::marketdata::domain::market_data_identifier parse(std::string_view s) {
    return oresmd_parser::parse(ores::marketdata::domain::oresmd_uri{std::string(s)});
}

}

/*
 * One test per row of id:C3E053CA-0D4B-480B-9119-E11530160EC1's "Worked examples"
 * section, checking the exact index name/curve key/quote key string that section
 * documents for each oresmd URI.
 */

TEST_CASE("fx_spot_quote_key", tags) {
    const auto id = parse("oresmd://fx/eurusd?type=quote&quote=spot");
    REQUIRE(oresmd_projections::to_quote_key(id) == "FX/RATE/EUR/USD");
    REQUIRE_FALSE(oresmd_projections::to_index_name(id).has_value());
    REQUIRE_FALSE(oresmd_projections::to_curve_key(id).has_value());
}

TEST_CASE("fx_quote_key_defaults_to_spot", tags) {
    const auto id = parse("oresmd://fx/eurusd?type=quote");
    REQUIRE(oresmd_projections::to_quote_key(id) == "FX/RATE/EUR/USD");
}

TEST_CASE("fx_fwd_quote_key", tags) {
    const auto id = parse("oresmd://fx/eurusd?type=quote&quote=fwd&point=6m");
    REQUIRE(oresmd_projections::to_quote_key(id) == "FXFWD/RATE/EUR/USD/6M");
}

TEST_CASE("fx_fwd_requires_point", tags) {
    const auto id = parse("oresmd://fx/eurusd?type=quote&quote=fwd");
    REQUIRE_FALSE(oresmd_projections::to_quote_key(id).has_value());
}

TEST_CASE("ir_usd_libor_3m_index_name_and_curve_key", tags) {
    const auto id = parse("oresmd://ir/usd?index=libor&tenor=3m&role=projection&type=fixing");
    REQUIRE(oresmd_projections::to_index_name(id) == "USD-LIBOR-3M");
    REQUIRE(oresmd_projections::to_curve_key(id) == "Yield/USD/USD3M");
    REQUIRE_FALSE(oresmd_projections::to_quote_key(id).has_value());
}

TEST_CASE("ir_usd_libor_6m_index_name_and_curve_key", tags) {
    const auto id = parse("oresmd://ir/usd?index=libor&tenor=6m&role=projection&type=fixing");
    REQUIRE(oresmd_projections::to_index_name(id) == "USD-LIBOR-6M");
    REQUIRE(oresmd_projections::to_curve_key(id) == "Yield/USD/USD6M");
}

TEST_CASE("ir_usd_sofr_index_name_drops_tenor_suffix_for_overnight_index", tags) {
    const auto id = parse("oresmd://ir/usd?index=sofr&tenor=1d&role=discount&type=fixing");
    REQUIRE(oresmd_projections::to_index_name(id) == "USD-SOFR");
    REQUIRE(oresmd_projections::to_curve_key(id) == "Yield/USD/USD1D");
}

TEST_CASE("ir_eur_euribor_6m_index_name_and_curve_key", tags) {
    const auto id = parse("oresmd://ir/eur?index=euribor&tenor=6m&role=projection&type=fixing");
    REQUIRE(oresmd_projections::to_index_name(id) == "EUR-EURIBOR-6M");
    REQUIRE(oresmd_projections::to_curve_key(id) == "Yield/EUR/EUR6M");
}

TEST_CASE("ir_eur_estr_index_name_drops_tenor_suffix_for_overnight_index", tags) {
    const auto id = parse("oresmd://ir/eur?index=estr&tenor=1d&role=discount&type=fixing");
    REQUIRE(oresmd_projections::to_index_name(id) == "EUR-ESTR");
    REQUIRE(oresmd_projections::to_curve_key(id) == "Yield/EUR/EUR1D");
}

TEST_CASE("ir_chf_saron_index_name_drops_tenor_suffix_for_new_rfr_family", tags) {
    // A family added by the SQL CHECK beyond the original 6
    // (synthetic_ir_curve_generation_configs_create.sql): is_overnight() must classify it as
    // overnight so the index name drops the tenor suffix, exactly as the original four overnight
    // families do.
    const auto id = parse("oresmd://ir/chf?index=saron&tenor=1d&role=discount&type=fixing");
    REQUIRE(oresmd_projections::to_index_name(id) == "CHF-SARON");
    REQUIRE(oresmd_projections::to_curve_key(id) == "Yield/CHF/CHF1D");
}

TEST_CASE("ir_swap_quote_key_matches_worked_example", tags) {
    const auto id = parse(
        "oresmd://ir/"
        "usd?index=libor&tenor=3m&role=projection&type=quote&quote=ir_swap&metric=rate&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "IR_SWAP/RATE/USD/2D/3M/5Y");
}

TEST_CASE("discount_quote_key_matches_worked_example", tags) {
    const auto id = parse("oresmd://ir/usd?index=libor&tenor=3m&role=projection&type=quote&"
                          "quote=discount&metric=rate&point=6m");
    REQUIRE(oresmd_projections::to_quote_key(id) == "DISCOUNT/RATE/USD/USD3M/6M");
}

TEST_CASE("swaption_vol_quote_key_matches_worked_example", tags) {
    const auto id = parse("oresmd://ir/eur?type=vol&point=5y,2y,atm");
    REQUIRE(oresmd_projections::to_quote_key(id) == "SWAPTION/RATE_LNVOL/EUR/5Y/2Y/ATM");
}

/*
 * New IR quote types covered by id:D566131C-D08C-4AFE-950E-B3DD26EB2C24
 * ("Extend oresmd to full ORE quote-type coverage"), one test per TYPE/METRIC pair.
 */

TEST_CASE("ir_mm_rate_quote_key", tags) {
    const auto id =
        parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=mm&metric=rate&point=1m");
    REQUIRE(oresmd_projections::to_quote_key(id) == "MM/RATE/EUR/EURIBOR/3M/1M");
}

TEST_CASE("ir_fra_rate_quote_key", tags) {
    const auto id =
        parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=fra&metric=rate&point=6m");
    REQUIRE(oresmd_projections::to_quote_key(id) == "FRA/RATE/EUR/EURIBOR/3M/6M");
}

TEST_CASE("ir_imm_fra_rate_quote_key", tags) {
    const auto id =
        parse("oresmd://ir/usd?index=libor&tenor=3m&type=quote&quote=imm_fra&metric=rate&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "IMM_FRA/RATE/USD/LIBOR/3M/5Y");
}

TEST_CASE("ir_basis_swap_spread_quote_key", tags) {
    const auto id = parse(
        "oresmd://ir/"
        "eur?index=euribor&tenor=3m&type=quote&quote=basis_swap&metric=basis_spread&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "BASIS_SWAP/BASIS_SPREAD/EUR/EURIBOR/3M/5Y");
}

TEST_CASE("ir_cc_basis_swap_spread_quote_key", tags) {
    const auto id = parse(
        "oresmd://ir/eur?tenor=3m&type=quote&quote=cc_basis_swap&metric=basis_spread&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "CC_BASIS_SWAP/BASIS_SPREAD/EUR/3M/5Y");
}

TEST_CASE("ir_cc_fix_float_swap_rate_quote_key", tags) {
    const auto id =
        parse("oresmd://ir/usd?tenor=3m&type=quote&quote=cc_fix_float_swap&metric=rate&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "CC_FIX_FLOAT_SWAP/RATE/USD/3M/5Y");
}

TEST_CASE("ir_bma_swap_ratio_quote_key", tags) {
    const auto id =
        parse("oresmd://ir/usd?tenor=3m&type=quote&quote=bma_swap&metric=ratio&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "BMA_SWAP/RATIO/USD/3M/5Y");
}

TEST_CASE("ir_zero_rate_quote_key", tags) {
    const auto id =
        parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=zero&metric=rate&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "ZERO/RATE/EUR/EURIBOR/3M/5Y");
}

TEST_CASE("ir_zero_yield_spread_quote_key", tags) {
    const auto id =
        parse("oresmd://ir/"
              "eur?index=euribor&tenor=3m&type=quote&quote=zero&metric=yield_spread&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "ZERO/YIELD_SPREAD/EUR/EURIBOR/3M/5Y");
}

TEST_CASE("ir_mm_future_price_quote_key", tags) {
    const auto id = parse(
        "oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=mm_future&metric=price&point=cme");
    REQUIRE(oresmd_projections::to_quote_key(id) == "MM_FUTURE/PRICE/EUR/EURIBOR/3M/CME");
}

TEST_CASE("ir_oi_future_price_quote_key", tags) {
    const auto id = parse(
        "oresmd://ir/usd?index=sofr&tenor=3m&type=quote&quote=oi_future&metric=price&point=cme");
    REQUIRE(oresmd_projections::to_quote_key(id) == "OI_FUTURE/PRICE/USD/SOFR/3M/CME");
}

/*
 * New-style quote= parameter with implicit metric (defaulted from the quote type).
 */

TEST_CASE("ir_mm_rate_metric_defaulted_from_quote_type", tags) {
    const auto id = parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=mm&point=1m");
    REQUIRE(oresmd_projections::to_quote_key(id) == "MM/RATE/EUR/EURIBOR/3M/1M");
}

TEST_CASE("ir_basis_swap_metric_defaulted_to_basis_spread", tags) {
    const auto id =
        parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=basis_swap&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "BASIS_SWAP/BASIS_SPREAD/EUR/EURIBOR/3M/5Y");
}

TEST_CASE("ir_mm_future_metric_defaulted_to_price", tags) {
    const auto id =
        parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=mm_future&point=cme");
    REQUIRE(oresmd_projections::to_quote_key(id) == "MM_FUTURE/PRICE/EUR/EURIBOR/3M/CME");
}

/*
 * quote_type is required for type=quote to produce a quote key.
 */

TEST_CASE("ir_quote_key_requires_quote_type", tags) {
    const auto id = parse("oresmd://ir/usd?index=libor&tenor=3m&type=quote&metric=rate&point=5y");
    REQUIRE_FALSE(oresmd_projections::to_quote_key(id).has_value());
}

/*
 * quote= requires type=quote (not fixing, not curve).
 */

TEST_CASE("reject_ir_quote_present_when_type_is_not_quote", tags) {
    REQUIRE_THROWS_AS(parse("oresmd://ir/usd?index=libor&tenor=3m&type=fixing&quote=ir_swap"),
                      ores::marketdata::core::oresmd_exception);
}

/*
 * quote= is IR-only; rejected for other asset classes.
 */

TEST_CASE("reject_fx_uri_with_ir_only_quote_field", tags) {
    REQUIRE_THROWS_AS(parse("oresmd://fx/eurusd?type=quote&quote=mm"),
                      ores::marketdata::core::oresmd_exception);
}

TEST_CASE("equity_spot_quote_key_matches_worked_example", tags) {
    const auto id = parse("oresmd://equity/aapl?ccy=usd&type=quote&quote=spot");
    REQUIRE(oresmd_projections::to_quote_key(id) == "EQUITY/PRICE/AAPL/USD");
}

TEST_CASE("equity_quote_key_defaults_to_spot", tags) {
    const auto id = parse("oresmd://equity/aapl?ccy=usd&type=quote");
    REQUIRE(oresmd_projections::to_quote_key(id) == "EQUITY/PRICE/AAPL/USD");
}

TEST_CASE("equity_dividend_quote_key", tags) {
    const auto id = parse("oresmd://equity/aapl?ccy=usd&type=quote&quote=dividend&point=1y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "EQUITY_DIVIDEND/RATE/AAPL/USD/1Y");
}

TEST_CASE("equity_fwd_quote_key", tags) {
    const auto id = parse("oresmd://equity/lufthansa?ccy=eur&type=quote&quote=fwd&point=6m");
    REQUIRE(oresmd_projections::to_quote_key(id) == "EQUITY_FWD/PRICE/LUFTHANSA/EUR/6M");
}

TEST_CASE("equity_dividend_requires_point", tags) {
    const auto id = parse("oresmd://equity/aapl?ccy=usd&type=quote&quote=dividend");
    REQUIRE_FALSE(oresmd_projections::to_quote_key(id).has_value());
}

TEST_CASE("cds_quote_key_matches_worked_example", tags) {
    const auto id = parse("oresmd://credit/itraxx-europe?ccy=eur&type=quote&quote=cds&point=sr,5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "CDS/CREDIT_SPREAD/ITRAXX-EUROPE/SR/EUR/5Y");
}

TEST_CASE("cds_quote_key_defaults_when_quote_absent", tags) {
    const auto id = parse("oresmd://credit/itraxx-europe?ccy=eur&type=quote&point=sr,5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "CDS/CREDIT_SPREAD/ITRAXX-EUROPE/SR/EUR/5Y");
}

TEST_CASE("hazard_rate_quote_key_matches_ore_format_6_segments", tags) {
    const auto id = parse("oresmd://credit/vod?ccy=eur&type=quote&quote=hazard_rate&point=sr,5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "HAZARD_RATE/RATE/VOD/SR/EUR/5Y");
}

TEST_CASE("recovery_rate_quote_key", tags) {
    const auto id = parse("oresmd://credit/vod?ccy=eur&type=quote&quote=recovery_rate&point=sr");
    REQUIRE(oresmd_projections::to_quote_key(id) == "RECOVERY_RATE/RATE/VOD/SR/EUR");
}

TEST_CASE("cds_index_base_correlation_quote_key_no_ccy", tags) {
    const auto id =
        parse("oresmd://credit/cdx-na-ig?ccy=usd&type=quote&quote=cds_index&point=5y,0.1");
    REQUIRE(oresmd_projections::to_quote_key(id) == "CDS_INDEX/BASE_CORRELATION/CDX-NA-IG/5Y/0.1");
}

TEST_CASE("index_cds_tranche_base_correlation_quote_key_no_ccy", tags) {
    const auto id =
        parse("oresmd://credit/2i65byeg6?ccy=usd&type=quote&quote=index_cds_tranche&point=5y,0.07");
    REQUIRE(oresmd_projections::to_quote_key(id) ==
            "INDEX_CDS_TRANCHE/BASE_CORRELATION/2I65BYEG6/5Y/0.07");
}

TEST_CASE("commodity_spot_quote_key", tags) {
    const auto id = parse("oresmd://commodity/gold?ccy=usd&type=quote&quote=spot");
    REQUIRE(oresmd_projections::to_quote_key(id) == "COMMODITY/PRICE/GOLD/USD");
}

TEST_CASE("commodity_quote_key_defaults_to_spot", tags) {
    const auto id = parse("oresmd://commodity/gold?ccy=usd&type=quote");
    REQUIRE(oresmd_projections::to_quote_key(id) == "COMMODITY/PRICE/GOLD/USD");
}

TEST_CASE("commodity_fwd_quote_key", tags) {
    const auto id = parse("oresmd://commodity/wti?ccy=usd&type=quote&quote=fwd&point=6m");
    REQUIRE(oresmd_projections::to_quote_key(id) == "COMMODITY_FWD/PRICE/WTI/USD/6M");
}

/*
 * Inflation asset class — new instrument family (id:D566131C-D08C-4AFE-950E-B3DD26EB2C24).
 */

TEST_CASE("zc_inflation_swap_quote_key", tags) {
    const auto id = parse("oresmd://inflation/ukrpi?type=quote&quote=zc_swap&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "ZC_INFLATIONSWAP/RATE/UKRPI/5Y");
}

TEST_CASE("yy_inflation_swap_quote_key", tags) {
    const auto id = parse("oresmd://inflation/ukrpi?type=quote&quote=yy_swap&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "YY_INFLATIONSWAP/RATE/UKRPI/5Y");
}

TEST_CASE("seasonality_quote_key", tags) {
    const auto id = parse("oresmd://inflation/ukrpi?type=quote&quote=seasonality&point=jan");
    REQUIRE(oresmd_projections::to_quote_key(id) == "SEASONALITY/RATE/MULT/UKRPI/JAN");
}

/*
 * Correlation asset class (id:D566131C-D08C-4AFE-950E-B3DD26EB2C24).
 */

TEST_CASE("correlation_pairwise_quote_key", tags) {
    const auto id = parse("oresmd://correlation/ccy-eur-usd?type=quote&quote=pairwise");
    REQUIRE(oresmd_projections::to_quote_key(id) == "CORRELATION/RATE/CCY-EUR-USD");
}

TEST_CASE("commodity_cpr_quote_key", tags) {
    const auto id = parse("oresmd://commodity/wti?ccy=usd&type=quote&quote=cpr&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "CPR/RATE/WTI/USD/5Y");
}

TEST_CASE("discount_vs_projection_gap_resolved_structurally", tags) {
    const auto projection =
        parse("oresmd://ir/eur?index=euribor&tenor=6m&role=projection&type=fixing");
    const auto discount = parse("oresmd://ir/eur?index=estr&tenor=1d&role=discount&type=fixing");
    REQUIRE(oresmd_projections::to_curve_key(projection) !=
            oresmd_projections::to_curve_key(discount));
}

/*
 * split_market_series_key() is the reverse of to_quote_key()/to_curve_key(): splits an
 * already-projected key string back into the three columns market_series stores them
 * under. Consolidates three previously-duplicated ad-hoc parsers
 * (ores.synthetic.service's feed_controller.hpp x2, ores.marketdata.service's
 * feed_ingest_loop.cpp) into one shared, tested implementation.
 */

TEST_CASE("split_market_series_key_splits_a_simple_fx_key", tags) {
    const auto key = oresmd_projections::split_market_series_key("FX/RATE/EUR/USD");
    REQUIRE(key.has_value());
    REQUIRE(key->series_type == "FX");
    REQUIRE(key->metric == "RATE");
    REQUIRE(key->qualifier == "EUR/USD");
}

TEST_CASE("split_market_series_key_absorbs_every_remaining_segment_into_qualifier", tags) {
    const auto key = oresmd_projections::split_market_series_key("IR_SWAP/RATE/USD/2D/3M/PAR_RATE");
    REQUIRE(key.has_value());
    REQUIRE(key->series_type == "IR_SWAP");
    REQUIRE(key->metric == "RATE");
    REQUIRE(key->qualifier == "USD/2D/3M/PAR_RATE");
}

TEST_CASE("split_market_series_key_round_trips_a_real_projected_quote_key", tags) {
    const auto id = parse("oresmd://fx/eurusd?type=quote");
    const auto projected = oresmd_projections::to_quote_key(id);
    REQUIRE(projected.has_value());
    const auto key = oresmd_projections::split_market_series_key(*projected);
    REQUIRE(key.has_value());
    REQUIRE(key->series_type == "FX");
    REQUIRE(key->metric == "RATE");
    REQUIRE(key->qualifier == "EUR/USD");
}

TEST_CASE("split_market_series_key_rejects_a_key_with_fewer_than_three_segments", tags) {
    REQUIRE_FALSE(oresmd_projections::split_market_series_key("FX/RATE").has_value());
    REQUIRE_FALSE(oresmd_projections::split_market_series_key("FX").has_value());
    REQUIRE_FALSE(oresmd_projections::split_market_series_key("").has_value());
}

/*
 * from_ore_key() — the inverse projection: an ORE quote key string becomes the oresmd
 * identifier the forward projection emitted it from. Each equality below pins the
 * inverse to the forward exactly: the identifier the inverse builds equals the one the
 * parser builds from the corresponding URI. Contract per acceptance: seeded by the
 * series_key_registry decomposition table; series types without an oresmd mapping are
 * rejected; the canonical URI the import boundary stores is to_uri() of the result.
 */

TEST_CASE("from_ore_key_fx_spot", tags) {
    REQUIRE(oresmd_projections::from_ore_key("FX/RATE/EUR/USD") ==
            parse("oresmd://fx/eurusd?type=quote&quote=spot"));
}

TEST_CASE("from_ore_key_fx_fwd", tags) {
    REQUIRE(oresmd_projections::from_ore_key("FXFWD/RATE/EUR/USD/6M") ==
            parse("oresmd://fx/eurusd?type=quote&quote=fwd&point=6m"));
}

TEST_CASE("from_ore_key_ir_swap_drops_the_settlement_segment", tags) {
    // The forward hardcodes "2D" into the settle slot; the inverse accepts any settle
    // and drops it — the identifier has no settle field.
    REQUIRE(oresmd_projections::from_ore_key("IR_SWAP/RATE/USD/2D/3M/5Y") ==
            parse("oresmd://ir/usd?tenor=3m&type=quote&quote=ir_swap&metric=rate&point=5y"));
    REQUIRE(oresmd_projections::from_ore_key("IR_SWAP/RATE/USD/1D/3M/5Y") ==
            parse("oresmd://ir/usd?tenor=3m&type=quote&quote=ir_swap&metric=rate&point=5y"));
}

TEST_CASE("from_ore_key_ir_discount_reverses_the_curve_id", tags) {
    // The forward emits CURVE_ID = CCY + TENOR; the inverse strips the ccy prefix back
    // into the tenor field.
    REQUIRE(oresmd_projections::from_ore_key("DISCOUNT/RATE/USD/USD3M/6M") ==
            parse("oresmd://ir/usd?tenor=3m&type=quote&quote=discount&metric=rate&point=6m"));
}

TEST_CASE("from_ore_key_ir_discount_rejects_a_curve_id_without_the_ccy_prefix", tags) {
    REQUIRE_FALSE(oresmd_projections::from_ore_key("DISCOUNT/RATE/USD/GBP3M/6M").has_value());
}

TEST_CASE("from_ore_key_ir_indexed_families", tags) {
    REQUIRE(oresmd_projections::from_ore_key("MM/RATE/EUR/EURIBOR/3M/1M") ==
            parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=mm&metric=rate&point=1m"));
    REQUIRE(oresmd_projections::from_ore_key("FRA/RATE/EUR/EURIBOR/3M/6M") ==
            parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=fra&metric=rate&point=6m"));
    REQUIRE(oresmd_projections::from_ore_key("IMM_FRA/RATE/USD/LIBOR/3M/5Y") ==
            parse("oresmd://ir/usd?index=libor&tenor=3m&type=quote&quote=imm_fra&metric=rate&point=5y"));
    REQUIRE(oresmd_projections::from_ore_key("BASIS_SWAP/BASIS_SPREAD/EUR/EURIBOR/3M/5Y") ==
            parse("oresmd://ir/"
                  "eur?index=euribor&tenor=3m&type=quote&quote=basis_swap&metric=basis_spread&point=5y"));
    REQUIRE(oresmd_projections::from_ore_key("ZERO/RATE/EUR/EURIBOR/3M/5Y") ==
            parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=zero&metric=rate&point=5y"));
    REQUIRE(oresmd_projections::from_ore_key("MM_FUTURE/PRICE/EUR/EURIBOR/3M/CME") ==
            parse("oresmd://ir/"
                  "eur?index=euribor&tenor=3m&type=quote&quote=mm_future&metric=price&point=cme"));
    REQUIRE(oresmd_projections::from_ore_key("OI_FUTURE/PRICE/USD/SOFR/3M/CME") ==
            parse("oresmd://ir/usd?index=sofr&tenor=3m&type=quote&quote=oi_future&metric=price&point=cme"));
}

TEST_CASE("from_ore_key_ir_no_index_families", tags) {
    REQUIRE(oresmd_projections::from_ore_key("CC_BASIS_SWAP/BASIS_SPREAD/EUR/3M/5Y") ==
            parse("oresmd://ir/eur?tenor=3m&type=quote&quote=cc_basis_swap&metric=basis_spread&point=5y"));
    REQUIRE(oresmd_projections::from_ore_key("CC_FIX_FLOAT_SWAP/RATE/USD/3M/5Y") ==
            parse("oresmd://ir/usd?tenor=3m&type=quote&quote=cc_fix_float_swap&metric=rate&point=5y"));
    REQUIRE(oresmd_projections::from_ore_key("BMA_SWAP/RATIO/USD/3M/5Y") ==
            parse("oresmd://ir/usd?tenor=3m&type=quote&quote=bma_swap&metric=ratio&point=5y"));
}

TEST_CASE("from_ore_key_ir_swaption_builds_the_vol_struct", tags) {
    REQUIRE(oresmd_projections::from_ore_key("SWAPTION/RATE_LNVOL/EUR/5Y/2Y/ATM") ==
            parse("oresmd://ir/eur?type=vol&point=5y,2y,atm"));
}

TEST_CASE("from_ore_key_equity", tags) {
    REQUIRE(oresmd_projections::from_ore_key("EQUITY/PRICE/AAPL/USD") ==
            parse("oresmd://equity/aapl?ccy=usd&type=quote&quote=spot"));
    REQUIRE(oresmd_projections::from_ore_key("EQUITY_FWD/PRICE/LUFTHANSA/EUR/6M") ==
            parse("oresmd://equity/lufthansa?ccy=eur&type=quote&quote=fwd&point=6m"));
    REQUIRE(oresmd_projections::from_ore_key("EQUITY_DIVIDEND/RATE/AAPL/USD/1Y") ==
            parse("oresmd://equity/aapl?ccy=usd&type=quote&quote=dividend&point=1y"));
}

TEST_CASE("from_ore_key_commodity", tags) {
    REQUIRE(oresmd_projections::from_ore_key("COMMODITY/PRICE/GOLD/USD") ==
            parse("oresmd://commodity/gold?ccy=usd&type=quote&quote=spot"));
    REQUIRE(oresmd_projections::from_ore_key("COMMODITY_FWD/PRICE/WTI/USD/6M") ==
            parse("oresmd://commodity/wti?ccy=usd&type=quote&quote=fwd&point=6m"));
    REQUIRE(oresmd_projections::from_ore_key("CPR/RATE/WTI/USD/5Y") ==
            parse("oresmd://commodity/wti?ccy=usd&type=quote&quote=cpr&point=5y"));
}

TEST_CASE("from_ore_key_credit", tags) {
    REQUIRE(oresmd_projections::from_ore_key("CDS/CREDIT_SPREAD/ITRAXX-EUROPE/SR/EUR/5Y") ==
            parse("oresmd://credit/itraxx-europe?ccy=eur&type=quote&quote=cds&point=sr,5y"));
    REQUIRE(oresmd_projections::from_ore_key("HAZARD_RATE/RATE/VOD/SR/EUR/5Y") ==
            parse("oresmd://credit/vod?ccy=eur&type=quote&quote=hazard_rate&point=sr,5y"));
    REQUIRE(oresmd_projections::from_ore_key("RECOVERY_RATE/RATE/VOD/SR/EUR") ==
            parse("oresmd://credit/vod?ccy=eur&type=quote&quote=recovery_rate&point=sr"));
    // CDS_INDEX/INDEX_CDS_TRANCHE keys carry no ccy segment (the forward drops it), so
    // the inverse cannot recover one -- the comparison target is a ccy-less identifier.
    credit_market_data_identifier index_id;
    index_id.reference_entity = "CDX-NA-IG";
    index_id.type = instrument_type::quote;
    index_id.quote_type = credit_quote_type::cds_index;
    index_id.point = "5y,0.1";
    REQUIRE(oresmd_projections::from_ore_key("CDS_INDEX/BASE_CORRELATION/CDX-NA-IG/5Y/0.1") ==
            market_data_identifier(index_id));
    credit_market_data_identifier tranche_id;
    tranche_id.reference_entity = "2I65BYEG6";
    tranche_id.type = instrument_type::quote;
    tranche_id.quote_type = credit_quote_type::index_cds_tranche;
    tranche_id.point = "5y,0.07";
    REQUIRE(oresmd_projections::from_ore_key("INDEX_CDS_TRANCHE/BASE_CORRELATION/2I65BYEG6/5Y/0.07") ==
            market_data_identifier(tranche_id));
}

TEST_CASE("from_ore_key_inflation", tags) {
    REQUIRE(oresmd_projections::from_ore_key("ZC_INFLATIONSWAP/RATE/UKRPI/5Y") ==
            parse("oresmd://inflation/ukrpi?type=quote&quote=zc_swap&point=5y"));
    REQUIRE(oresmd_projections::from_ore_key("YY_INFLATIONSWAP/RATE/UKRPI/5Y") ==
            parse("oresmd://inflation/ukrpi?type=quote&quote=yy_swap&point=5y"));
    // The forward emits the literal MULT in the third segment; the inverse accepts it
    // and drops it.
    REQUIRE(oresmd_projections::from_ore_key("SEASONALITY/RATE/MULT/UKRPI/JAN") ==
            parse("oresmd://inflation/ukrpi?type=quote&quote=seasonality&point=jan"));
}

TEST_CASE("from_ore_key_correlation", tags) {
    REQUIRE(oresmd_projections::from_ore_key("CORRELATION/RATE/CCY-EUR-USD") ==
            parse("oresmd://correlation/ccy-eur-usd?type=quote&quote=pairwise"));
}

TEST_CASE("from_ore_key_semantically_equal_identifiers_for_case_variants", tags) {
    // The inverse normalises like the parser: codes/ccy upper, tenor/point lower —
    // a lowercase key is the same identity as its uppercase form.
    REQUIRE(oresmd_projections::from_ore_key("FX/RATE/eur/usd") ==
            oresmd_projections::from_ore_key("FX/RATE/EUR/USD"));
    REQUIRE(oresmd_projections::from_ore_key("MM/RATE/eur/euribor/3m/1m") ==
            oresmd_projections::from_ore_key("MM/RATE/EUR/EURIBOR/3M/1M"));
}

TEST_CASE("from_ore_key_pins_the_canonical_uri_of_the_import_boundary", tags) {
    // The stored string is to_uri() of the inverse's identifier — the same canonical
    // form every other producer uses, so matching compares like with like.
    REQUIRE(oresmd_parser::to_uri(*oresmd_projections::from_ore_key("FX/RATE/EUR/USD")).value ==
            "oresmd://fx/eurusd?type=quote&quote=spot");
    REQUIRE(oresmd_parser::to_uri(*oresmd_projections::from_ore_key("IR_SWAP/RATE/USD/2D/3M/5Y"))
                .value == "oresmd://ir/usd?tenor=3m&type=quote&metric=rate&quote=ir_swap&point=5y");
    REQUIRE(oresmd_parser::to_uri(*oresmd_projections::from_ore_key("CDS/CREDIT_SPREAD/VOD/SR/EUR/5Y"))
                .value == "oresmd://credit/vod?ccy=eur&type=quote&quote=cds&point=sr,5y");
}

TEST_CASE("from_ore_key_fx_spot_convention_correction_swaps_the_pair", tags) {
    // The FX/RATE correction survives the cutover: same fx_quote_convention_checker,
    // new target — the identifier's pair is swapped at parse time.
    const ores::ore::market::fx_quote_convention_checker checker({{"EUR", "USD"}});
    const auto swapped =
        oresmd_projections::from_ore_key("FX/RATE/USD/EUR", checker);
    REQUIRE(swapped.has_value());
    REQUIRE(std::get<fx_market_data_identifier>(*swapped).pair == "EURUSD");
    // Without the checker the pair stays in key order.
    const auto as_is = oresmd_projections::from_ore_key("FX/RATE/USD/EUR");
    REQUIRE(as_is.has_value());
    REQUIRE(std::get<fx_market_data_identifier>(*as_is).pair == "USDEUR");
}

TEST_CASE("from_ore_key_fx_spot_convention_correction_leaves_an_unknown_pair_alone", tags) {
    const ores::ore::market::fx_quote_convention_checker checker({{"EUR", "USD"}});
    const auto id = oresmd_projections::from_ore_key("FX/RATE/GBP/JPY", checker);
    REQUIRE(id.has_value());
    REQUIRE(std::get<fx_market_data_identifier>(*id).pair == "GBPJPY");
}

TEST_CASE("from_ore_key_convention_correction_only_touches_fx_spot", tags) {
    const ores::ore::market::fx_quote_convention_checker checker({{"EUR", "USD"}});
    // A fwd key with a reversed pair is not corrected — the boundary only ever
    // checked FX/RATE spot keys, and the inverse preserves that behaviour.
    const auto id = oresmd_projections::from_ore_key("FXFWD/RATE/USD/EUR/6M", checker);
    REQUIRE(id.has_value());
    REQUIRE(std::get<fx_market_data_identifier>(*id).pair == "USDEUR");
}

/*
 * Rejection: series types without an oresmd mapping (the registry's option/capfloor
 * families, BOND), types the registry does not know, and malformed keys.
 */

TEST_CASE("from_ore_key_rejects_registry_types_without_an_oresmd_mapping", tags) {
    REQUIRE_FALSE(oresmd_projections::from_ore_key("BOND/PRICE/ISINXS/SNR/5Y").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("FX_OPTION/RATE/EUR/USD/1Y/25D").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("CAPFLOOR/RATE/USD/5Y/3M").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("INDEX_CDS_OPTION/RATE/CDX/5Y/1Y").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("EQUITY_OPTION/PRICE/AAPL/USD/1Y/100").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("COMMODITY_OPTION/PRICE/WTI/USD/1Y/100").has_value());
    REQUIRE_FALSE(
        oresmd_projections::from_ore_key("ZC_INFLATIONCAPFLOOR/RATE/UKRPI/5Y").has_value());
    REQUIRE_FALSE(
        oresmd_projections::from_ore_key("YY_INFLATIONCAPFLOOR/RATE/UKRPI/5Y").has_value());
}

TEST_CASE("from_ore_key_rejects_unknown_series_types", tags) {
    // "FIXING" is the index-name key space of the fixing boundary, not a registry
    // series type; the reverse of to_index_name() lands with the identity-core task.
    REQUIRE_FALSE(oresmd_projections::from_ore_key("FIXING/RATE/EUR-EONIA").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("BOGUS/RATE/EUR/USD").has_value());
}

TEST_CASE("from_ore_key_rejects_malformed_keys", tags) {
    // Fewer than three segments.
    REQUIRE_FALSE(oresmd_projections::from_ore_key("FX/RATE").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("FX").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("").has_value());
    // Wrong segment count for the type.
    REQUIRE_FALSE(oresmd_projections::from_ore_key("FX/RATE/EUR").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("FX/RATE/EUR/USD/EXTRA").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("IR_SWAP/RATE/USD/2D/3M").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("SWAPTION/RATE_LNVOL/EUR/5Y/2Y").has_value());
    // Empty segments.
    REQUIRE_FALSE(oresmd_projections::from_ore_key("FX/RATE//USD").has_value());
    // A metric the forward never emits for the type.
    REQUIRE_FALSE(oresmd_projections::from_ore_key("FX/PRICE/EUR/USD").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("EQUITY_DIVIDEND/PRICE/AAPL/USD/1Y").has_value());
    // An unrecognised index family, metric, or vol model.
    REQUIRE_FALSE(oresmd_projections::from_ore_key("MM/RATE/EUR/BOGUS/3M/1M").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("IR_SWAP/BOGUS/USD/2D/3M/5Y").has_value());
    REQUIRE_FALSE(oresmd_projections::from_ore_key("SWAPTION/BOGUS/EUR/5Y/2Y/ATM").has_value());
}
