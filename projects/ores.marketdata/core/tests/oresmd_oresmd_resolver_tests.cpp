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
#include "ores.marketdata.core/oresmd/oresmd_resolver.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

using namespace ores::marketdata::domain;
using ores::marketdata::core::oresmd_exception;
using ores::marketdata::core::oresmd_resolver;

const std::string tags("[oresmd][resolver]");

}

TEST_CASE("resolve_ir_requirement_fills_unset_fields_from_defaults", tags) {
    ir_market_data_requirement req;
    req.ccy = "USD";
    req.tenor = "3m";
    // index/role/type/metric/point left unset -- must come from defaults.

    ir_market_data_identifier defaults;
    defaults.ccy = "EUR"; // requirement's ccy must win over this.
    defaults.type = instrument_type::fixing;
    defaults.index = index_family::libor;
    defaults.role = curve_role::projection;

    const auto resolved = oresmd_resolver::resolve(req, defaults);
    const auto& ir = std::get<ir_market_data_identifier>(resolved);
    REQUIRE(ir.ccy == "USD");                    // from the requirement, not the defaults.
    REQUIRE(ir.tenor == "3m");                   // from the requirement.
    REQUIRE(ir.type == instrument_type::fixing); // from defaults.
    REQUIRE(ir.index == index_family::libor);    // from defaults.
    REQUIRE(ir.role == curve_role::projection);  // from defaults.
    REQUIRE_FALSE(ir.metric.has_value());
    REQUIRE_FALSE(ir.point.has_value());
}

TEST_CASE("resolve_fully_specified_requirement_ignores_defaults", tags) {
    fx_market_data_requirement req;
    req.pair = "EURUSD";
    req.type = instrument_type::quote;

    fx_market_data_identifier defaults;
    defaults.pair = "GBPUSD";

    const auto resolved = oresmd_resolver::resolve(req, defaults);
    REQUIRE(std::get<fx_market_data_identifier>(resolved).pair == "EURUSD");
}

TEST_CASE("resolve_throws_when_mandatory_field_unset_in_both_requirement_and_defaults", tags) {
    equity_market_data_requirement req;
    req.ccy = "USD"; // ticker left unset.

    equity_market_data_identifier defaults;
    defaults.ccy = "USD"; // also no ticker.

    REQUIRE_THROWS_AS(oresmd_resolver::resolve(req, defaults), oresmd_exception);
}

TEST_CASE("resolve_throws_when_defaults_are_a_different_asset_class", tags) {
    // A fx default cannot narrow an ir requirement -- ccy is unresolved in either.
    ir_market_data_requirement req;
    req.tenor = "3m";

    fx_market_data_identifier mismatched_defaults;
    mismatched_defaults.pair = "EURUSD";

    market_data_identifier defaults = mismatched_defaults;
    REQUIRE_THROWS_AS(oresmd_resolver::resolve(req, defaults), oresmd_exception);
}

TEST_CASE("resolve_credit_requirement_with_all_fields_from_defaults", tags) {
    credit_market_data_requirement req; // everything left unset.

    credit_market_data_identifier defaults;
    defaults.reference_entity = "ITRAXX-EUROPE";
    defaults.ccy = "EUR";
    defaults.type = instrument_type::quote;
    defaults.point = "sr,5y";

    const auto resolved = oresmd_resolver::resolve(req, defaults);
    const auto& cr = std::get<credit_market_data_identifier>(resolved);
    REQUIRE(cr.reference_entity == "ITRAXX-EUROPE");
    REQUIRE(cr.ccy == "EUR");
    REQUIRE(cr.point == "sr,5y");
}

TEST_CASE("resolve_commodity_requirement_missing_commodity_code_throws", tags) {
    commodity_market_data_requirement req;
    req.ccy = "USD";

    commodity_market_data_identifier defaults;
    defaults.ccy = "USD";
    // commodity_code unset in both -- must throw, not default to an empty string.

    REQUIRE_THROWS_AS(oresmd_resolver::resolve(req, defaults), oresmd_exception);
}

/*
 * Requiredness follows the entity kind in the resolver as well as in the parser.
 * A requirement is a partial description, so the resolver is where a caller that
 * never said what a quote is denominated in has to be told; a fixing is not
 * asked, because an index name does not carry a currency.
 */

TEST_CASE("resolve_equity_quote_requirement_missing_ccy_throws", tags) {
    equity_market_data_requirement req;
    req.ticker = "SP5";
    req.type = instrument_type::quote;
    // ccy unset in both the requirement and the defaults.

    equity_market_data_identifier defaults;
    defaults.type = instrument_type::quote;

    REQUIRE_THROWS_AS(oresmd_resolver::resolve(req, defaults), oresmd_exception);
}

TEST_CASE("resolve_commodity_quote_requirement_missing_ccy_throws", tags) {
    commodity_market_data_requirement req;
    req.commodity_code = "GOLD";
    req.type = instrument_type::quote;

    commodity_market_data_identifier defaults;
    defaults.type = instrument_type::quote;

    REQUIRE_THROWS_AS(oresmd_resolver::resolve(req, defaults), oresmd_exception);
}

TEST_CASE("resolve_equity_fixing_requirement_needs_no_ccy", tags) {
    equity_market_data_requirement req;
    req.ticker = "SP5";
    req.type = instrument_type::fixing;

    equity_market_data_identifier defaults;
    defaults.type = instrument_type::fixing;

    const auto id = oresmd_resolver::resolve(req, defaults);
    const auto& eq = std::get<equity_market_data_identifier>(id);
    CHECK(eq.ticker == "SP5");
    CHECK(eq.type == instrument_type::fixing);
    CHECK_FALSE(eq.ccy.has_value());
}

TEST_CASE("resolve_commodity_fixing_requirement_needs_no_ccy", tags) {
    commodity_market_data_requirement req;
    req.commodity_code = "GOLD";
    req.type = instrument_type::fixing;

    commodity_market_data_identifier defaults;
    defaults.type = instrument_type::fixing;

    const auto id = oresmd_resolver::resolve(req, defaults);
    const auto& co = std::get<commodity_market_data_identifier>(id);
    CHECK(co.commodity_code == "GOLD");
    CHECK(co.type == instrument_type::fixing);
    CHECK_FALSE(co.ccy.has_value());
}
