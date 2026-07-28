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
#include <catch2/catch_test_macros.hpp>

namespace {

using namespace ores::marketdata::domain;
using ores::marketdata::core::oresmd_exception;
using ores::marketdata::core::oresmd_parser;

const std::string tags("[oresmd][parser]");

oresmd_uri uri(std::string_view s) {
    return oresmd_uri{std::string(s)};
}

}

/*
 * One test per worked example in id:C3E053CA-0D4B-480B-9119-E11530160EC1's
 * "Worked examples" section, matching that doc's tables exactly.
 */

TEST_CASE("parse_fx_spot_quote", tags) {
    const auto id = oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote"));
    const auto& fx = std::get<fx_market_data_identifier>(id);
    REQUIRE(fx.pair == "EURUSD");
    REQUIRE(fx.type == instrument_type::quote);
}

TEST_CASE("parse_ir_usd_libor_3m_fixing", tags) {
    const auto id = oresmd_parser::parse(
        uri("oresmd://ir/usd?index=libor&tenor=3m&role=projection&type=fixing"));
    const auto& ir = std::get<ir_market_data_identifier>(id);
    REQUIRE(ir.ccy == "USD");
    REQUIRE(ir.type == instrument_type::fixing);
    REQUIRE(ir.index == index_family::libor);
    REQUIRE(ir.tenor == "3m");
    REQUIRE(ir.role == curve_role::projection);
}

TEST_CASE("parse_ir_usd_libor_6m_fixing_is_structurally_distinct_from_3m", tags) {
    const auto id3 = oresmd_parser::parse(
        uri("oresmd://ir/usd?index=libor&tenor=3m&role=projection&type=fixing"));
    const auto id6 = oresmd_parser::parse(
        uri("oresmd://ir/usd?index=libor&tenor=6m&role=projection&type=fixing"));
    REQUIRE(std::get<ir_market_data_identifier>(id3).tenor !=
            std::get<ir_market_data_identifier>(id6).tenor);
}

TEST_CASE("parse_ir_usd_sofr_discount_fixing", tags) {
    const auto id =
        oresmd_parser::parse(uri("oresmd://ir/usd?index=sofr&tenor=1d&role=discount&type=fixing"));
    const auto& ir = std::get<ir_market_data_identifier>(id);
    REQUIRE(ir.ccy == "USD");
    REQUIRE(ir.index == index_family::sofr);
    REQUIRE(ir.role == curve_role::discount);
}

TEST_CASE("parse_ir_eur_euribor_6m_fixing", tags) {
    const auto id = oresmd_parser::parse(
        uri("oresmd://ir/eur?index=euribor&tenor=6m&role=projection&type=fixing"));
    const auto& ir = std::get<ir_market_data_identifier>(id);
    REQUIRE(ir.ccy == "EUR");
    REQUIRE(ir.index == index_family::euribor);
}

TEST_CASE("parse_ir_eur_estr_discount_fixing", tags) {
    const auto id =
        oresmd_parser::parse(uri("oresmd://ir/eur?index=estr&tenor=1d&role=discount&type=fixing"));
    const auto& ir = std::get<ir_market_data_identifier>(id);
    REQUIRE(ir.ccy == "EUR");
    REQUIRE(ir.index == index_family::estr);
    REQUIRE(ir.role == curve_role::discount);
}

TEST_CASE("parse_ir_par_rate_quote_disambiguated_by_metric", tags) {
    const auto id = oresmd_parser::parse(
        uri("oresmd://ir/"
            "usd?index=libor&tenor=3m&role=projection&type=quote&metric=par_rate&point=5y"));
    const auto& ir = std::get<ir_market_data_identifier>(id);
    REQUIRE(ir.type == instrument_type::quote);
    REQUIRE(ir.metric == metric::par_rate);
    REQUIRE(ir.point == "5y");
}

TEST_CASE("parse_ir_discount_factor_quote_disambiguated_by_metric", tags) {
    const auto id = oresmd_parser::parse(uri("oresmd://ir/usd?index=libor&tenor=3m&role=projection&"
                                             "type=quote&metric=discount_factor&point=6m"));
    const auto& ir = std::get<ir_market_data_identifier>(id);
    REQUIRE(ir.metric == metric::discount_factor);
    REQUIRE(ir.point == "6m");
}

TEST_CASE("parse_swaption_vol_point_has_no_index_tenor_role", tags) {
    const auto id = oresmd_parser::parse(uri("oresmd://ir/eur?type=vol&point=5y,2y,atm"));
    const auto& ir = std::get<ir_market_data_identifier>(id);
    REQUIRE(ir.ccy == "EUR");
    REQUIRE(ir.type == instrument_type::vol);
    REQUIRE_FALSE(ir.index.has_value());
    REQUIRE_FALSE(ir.tenor.has_value());
    REQUIRE_FALSE(ir.role.has_value());
    REQUIRE(ir.point == "5y,2y,atm");
}

TEST_CASE("parse_equity_quote", tags) {
    const auto id = oresmd_parser::parse(uri("oresmd://equity/aapl?ccy=usd&type=quote"));
    const auto& eq = std::get<equity_market_data_identifier>(id);
    REQUIRE(eq.ticker == "AAPL");
    REQUIRE(eq.ccy == "USD");
}

TEST_CASE("parse_credit_quote", tags) {
    const auto id =
        oresmd_parser::parse(uri("oresmd://credit/itraxx-europe?ccy=eur&type=quote&point=sr,5y"));
    const auto& cr = std::get<credit_market_data_identifier>(id);
    REQUIRE(cr.reference_entity == "ITRAXX-EUROPE");
    REQUIRE(cr.ccy == "EUR");
    REQUIRE(cr.point == "sr,5y");
}

TEST_CASE("parse_commodity_quote", tags) {
    const auto id = oresmd_parser::parse(uri("oresmd://commodity/gold?ccy=usd&type=quote"));
    const auto& co = std::get<commodity_market_data_identifier>(id);
    REQUIRE(co.commodity_code == "GOLD");
    REQUIRE(co.ccy == "USD");
}

/*
 * Round-trips: to_uri(parse(uri)) reparses to an identical identifier.
 */

TEST_CASE("round_trip_fx", tags) {
    const auto original = oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_ir_quote", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/"
            "usd?index=libor&tenor=3m&role=projection&type=quote&metric=par_rate&point=5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_equity", tags) {
    const auto original = oresmd_parser::parse(uri("oresmd://equity/aapl?ccy=usd&type=quote"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_credit", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://credit/itraxx-europe?ccy=eur&type=quote&point=sr,5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_commodity", tags) {
    const auto original = oresmd_parser::parse(uri("oresmd://commodity/gold?ccy=usd&type=quote"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

/*
 * Invalid cases: a field that belongs to a different asset class must be rejected,
 * not silently ignored -- see id:C3E053CA-0D4B-480B-9119-E11530160EC1's
 * per-asset-class conditionality.
 */

TEST_CASE("reject_fx_uri_with_ir_only_tenor_field", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote&tenor=3m")),
                      oresmd_exception);
}

TEST_CASE("reject_fx_uri_with_ir_only_role_field", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote&role=discount")),
                      oresmd_exception);
}

TEST_CASE("reject_fx_uri_with_ir_only_index_field", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote&index=libor")),
                      oresmd_exception);
}

TEST_CASE("reject_fx_uri_with_ccy_query_key", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote&ccy=usd")),
                      oresmd_exception);
}

TEST_CASE("reject_ir_uri_with_ccy_query_key_since_entity_already_is_the_currency", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://ir/usd?type=fixing&index=sofr&ccy=usd")),
                      oresmd_exception);
}

TEST_CASE("reject_ir_metric_present_when_type_is_not_quote", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(
                          uri("oresmd://ir/usd?index=libor&tenor=3m&type=fixing&metric=par_rate")),
                      oresmd_exception);
}

TEST_CASE("reject_equity_uri_with_ir_only_tenor_field", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://equity/aapl?ccy=usd&type=quote&tenor=3m")),
                      oresmd_exception);
}

TEST_CASE("reject_equity_uri_with_point_field", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://equity/aapl?ccy=usd&type=quote&point=5y")),
                      oresmd_exception);
}

TEST_CASE("reject_equity_uri_missing_mandatory_ccy", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://equity/aapl?type=quote")),
                      oresmd_exception);
}

TEST_CASE("reject_credit_uri_with_ir_only_index_field", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://credit/itraxx-europe?ccy=eur&type=quote&index=libor")),
        oresmd_exception);
}

TEST_CASE("reject_credit_uri_missing_mandatory_ccy", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://credit/itraxx-europe?type=quote&point=sr,5y")),
        oresmd_exception);
}

TEST_CASE("reject_commodity_uri_with_ir_only_role_field", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://commodity/gold?ccy=usd&type=quote&role=discount")),
        oresmd_exception);
}

TEST_CASE("reject_commodity_uri_missing_mandatory_ccy", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://commodity/gold?type=quote")),
                      oresmd_exception);
}

TEST_CASE("reject_unrecognised_scheme", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("https://fx/eurusd?type=quote")), oresmd_exception);
}

TEST_CASE("reject_unrecognised_asset_class", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://bogus/eurusd?type=quote")),
                      oresmd_exception);
}

TEST_CASE("reject_unrecognised_index_family_value", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://ir/usd?index=bogus&tenor=3m&type=fixing")),
                      oresmd_exception);
}

TEST_CASE("reject_unrecognised_instrument_type_value", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://fx/eurusd?type=bogus")), oresmd_exception);
}

TEST_CASE("reject_malformed_uri", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("not a uri at all")), oresmd_exception);
}
