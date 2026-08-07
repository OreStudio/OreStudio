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
#include <catch2/catch_test_macros.hpp>

namespace {

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

TEST_CASE("fx_quote_key_matches_worked_example", tags) {
    const auto id = parse("oresmd://fx/eurusd?type=quote");
    REQUIRE(oresmd_projections::to_quote_key(id) == "FX/RATE/EUR/USD");
    REQUIRE_FALSE(oresmd_projections::to_index_name(id).has_value());
    REQUIRE_FALSE(oresmd_projections::to_curve_key(id).has_value());
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

TEST_CASE("ir_par_rate_quote_key_matches_worked_example", tags) {
    const auto id = parse(
        "oresmd://ir/usd?index=libor&tenor=3m&role=projection&type=quote&metric=par_rate&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "IR_SWAP/RATE/USD/2D/3M/5Y");
}

TEST_CASE("ir_discount_factor_quote_key_matches_worked_example", tags) {
    const auto id = parse("oresmd://ir/usd?index=libor&tenor=3m&role=projection&type=quote&"
                          "metric=discount_factor&point=6m");
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
    const auto id = parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=mm&metric=rate&point=1m");
    REQUIRE(oresmd_projections::to_quote_key(id) == "MM/RATE/EUR/EURIBOR/3M/1M");
}

TEST_CASE("ir_fra_rate_quote_key", tags) {
    const auto id = parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=fra&metric=rate&point=6m");
    REQUIRE(oresmd_projections::to_quote_key(id) == "FRA/RATE/EUR/EURIBOR/3M/6M");
}

TEST_CASE("ir_imm_fra_rate_quote_key", tags) {
    const auto id = parse("oresmd://ir/usd?index=libor&tenor=3m&type=quote&quote=imm_fra&metric=rate&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "IMM_FRA/RATE/USD/LIBOR/3M/5Y");
}

TEST_CASE("ir_basis_swap_spread_quote_key", tags) {
    const auto id = parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=basis_swap&metric=basis_spread&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "BASIS_SWAP/BASIS_SPREAD/EUR/EURIBOR/3M/5Y");
}

TEST_CASE("ir_cc_basis_swap_spread_quote_key", tags) {
    const auto id = parse("oresmd://ir/eur?tenor=3m&type=quote&quote=cc_basis_swap&metric=basis_spread&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "CC_BASIS_SWAP/BASIS_SPREAD/EUR/3M/5Y");
}

TEST_CASE("ir_cc_fix_float_swap_rate_quote_key", tags) {
    const auto id = parse("oresmd://ir/usd?tenor=3m&type=quote&quote=cc_fix_float_swap&metric=rate&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "CC_FIX_FLOAT_SWAP/RATE/USD/3M/5Y");
}

TEST_CASE("ir_bma_swap_ratio_quote_key", tags) {
    const auto id = parse("oresmd://ir/usd?tenor=3m&type=quote&quote=bma_swap&metric=ratio&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "BMA_SWAP/RATIO/USD/3M/5Y");
}

TEST_CASE("ir_zero_rate_quote_key", tags) {
    const auto id = parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=zero&metric=rate&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "ZERO/RATE/EUR/EURIBOR/3M/5Y");
}

TEST_CASE("ir_zero_yield_spread_quote_key", tags) {
    const auto id = parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=zero&metric=yield_spread&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "ZERO/YIELD_SPREAD/EUR/EURIBOR/3M/5Y");
}

TEST_CASE("ir_mm_future_price_quote_key", tags) {
    const auto id = parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=mm_future&metric=price&point=cme");
    REQUIRE(oresmd_projections::to_quote_key(id) == "MM_FUTURE/PRICE/EUR/EURIBOR/3M/CME");
}

TEST_CASE("ir_oi_future_price_quote_key", tags) {
    const auto id = parse("oresmd://ir/usd?index=sofr&tenor=3m&type=quote&quote=oi_future&metric=price&point=cme");
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
    const auto id = parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=basis_swap&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "BASIS_SWAP/BASIS_SPREAD/EUR/EURIBOR/3M/5Y");
}

TEST_CASE("ir_mm_future_metric_defaulted_to_price", tags) {
    const auto id = parse("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=mm_future&point=cme");
    REQUIRE(oresmd_projections::to_quote_key(id) == "MM_FUTURE/PRICE/EUR/EURIBOR/3M/CME");
}

/*
 * Backward-compatible old-style metric-only URIs still produce correct quote keys.
 */

TEST_CASE("ir_old_style_par_rate_still_produces_ir_swap_quote_key", tags) {
    const auto id = parse("oresmd://ir/usd?index=libor&tenor=3m&role=projection&type=quote&metric=par_rate&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "IR_SWAP/RATE/USD/2D/3M/5Y");
}

TEST_CASE("ir_old_style_discount_factor_still_produces_discount_quote_key", tags) {
    const auto id = parse("oresmd://ir/usd?index=libor&tenor=3m&role=projection&type=quote&metric=discount_factor&point=6m");
    REQUIRE(oresmd_projections::to_quote_key(id) == "DISCOUNT/RATE/USD/USD3M/6M");
}

/*
 * New-style explicit quote= parameter with the same semantics as the old metric-only URIs.
 */

TEST_CASE("ir_new_style_ir_swap_rate_equivalent_to_old_par_rate", tags) {
    const auto old_id = parse("oresmd://ir/usd?index=libor&tenor=3m&role=projection&type=quote&metric=par_rate&point=5y");
    const auto new_id = parse("oresmd://ir/usd?index=libor&tenor=3m&role=projection&type=quote&quote=ir_swap&metric=rate&point=5y");
    REQUIRE(oresmd_projections::to_quote_key(new_id) == oresmd_projections::to_quote_key(old_id));
}

TEST_CASE("ir_new_style_discount_rate_equivalent_to_old_discount_factor", tags) {
    const auto old_id = parse("oresmd://ir/usd?index=libor&tenor=3m&role=projection&type=quote&metric=discount_factor&point=6m");
    const auto new_id = parse("oresmd://ir/usd?index=libor&tenor=3m&role=projection&type=quote&quote=discount&metric=rate&point=6m");
    REQUIRE(oresmd_projections::to_quote_key(new_id) == oresmd_projections::to_quote_key(old_id));
}

/*
 * quote= requires type=quote (not fixing, not curve).
 */

TEST_CASE("reject_ir_quote_present_when_type_is_not_quote", tags) {
    REQUIRE_THROWS_AS(
        parse("oresmd://ir/usd?index=libor&tenor=3m&type=fixing&quote=ir_swap"),
        ores::marketdata::core::oresmd_exception);
}

/*
 * quote= is IR-only; rejected for other asset classes.
 */

TEST_CASE("reject_fx_uri_with_ir_only_quote_field", tags) {
    REQUIRE_THROWS_AS(
        parse("oresmd://fx/eurusd?type=quote&quote=mm"),
        ores::marketdata::core::oresmd_exception);
}

TEST_CASE("equity_quote_key_matches_worked_example", tags) {
    const auto id = parse("oresmd://equity/aapl?ccy=usd&type=quote");
    REQUIRE(oresmd_projections::to_quote_key(id) == "EQUITY/PRICE/AAPL/USD");
}

TEST_CASE("credit_quote_key_matches_worked_example", tags) {
    const auto id = parse("oresmd://credit/itraxx-europe?ccy=eur&type=quote&point=sr,5y");
    REQUIRE(oresmd_projections::to_quote_key(id) == "CDS/CREDIT_SPREAD/ITRAXX-EUROPE/SR/EUR/5Y");
}

TEST_CASE("commodity_quote_key_matches_worked_example", tags) {
    const auto id = parse("oresmd://commodity/gold?ccy=usd&type=quote");
    REQUIRE(oresmd_projections::to_quote_key(id) == "COMMODITY/PRICE/GOLD/USD");
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
