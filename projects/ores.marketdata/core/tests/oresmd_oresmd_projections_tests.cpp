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
