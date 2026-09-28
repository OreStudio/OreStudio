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
 * Template: oresmd_parser_tests.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.marketdata.core/oresmd/oresmd_exception.hpp"
#include "ores.marketdata.core/oresmd/oresmd_parser.hpp"
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include <catch2/catch_test_macros.hpp>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::domain;
using ores::marketdata::core::canonical_values;
using ores::marketdata::core::oresmd_exception;
using ores::marketdata::core::oresmd_parser;
using ores::marketdata::core::oresmd_projections;

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

TEST_CASE("every_index_family_parses_a_fixing_the_way_its_index_name_is_written", tags) {
    // Generated from the Index family table of ir_quote_type.org, so a family
    // added to the model is covered here without anyone editing this list.
    //
    // A family ORE writes bare parses without a tenor. A family whose index name
    // always carries one is refused without it, and parses with it. The
    // classification is a property of the index name and not of the
    // curve-configuration CHECK, which answers an empty tenor for shibor, taibor
    // and tiie while their names here are CNY-SHIBOR-3M, TWD-TAIBOR-3M and
    // MXN-TIIE-28D.
    const std::vector<std::pair<std::string, index_family>> overnight{
        {"sofr", index_family::sofr},         {"estr", index_family::estr},
        {"sonia", index_family::sonia},       {"tona", index_family::tona},
        {"saron", index_family::saron},       {"aonia", index_family::aonia},
        {"corra", index_family::corra},       {"honia", index_family::honia},
        {"sora", index_family::sora},         {"swestr", index_family::swestr},
        {"nowa", index_family::nowa},         {"kofr", index_family::kofr},
        {"mibor", index_family::mibor},       {"zaronia", index_family::zaronia},
        {"destr", index_family::destr},       {"polonia", index_family::polonia},
        {"nzonia", index_family::nzonia},     {"ftiie", index_family::ftiie},
        {"camara", index_family::camara},     {"czeonia", index_family::czeonia},
        {"dkkois", index_family::dkkois},     {"eonia", index_family::eonia},
        {"fedfunds", index_family::fedfunds}, {"sifma", index_family::sifma},
        {"sior", index_family::sior},
    };
    REQUIRE_FALSE(overnight.empty());
    for (const auto& [name, expected] : overnight) {
        const auto id = oresmd_parser::parse(uri("oresmd://ir/usd?index=" + name + "&type=fixing"));
        const auto& ir = std::get<ir_market_data_identifier>(id);
        REQUIRE(ir.type == instrument_type::fixing);
        REQUIRE(ir.index == expected);
        REQUIRE_FALSE(ir.tenor.has_value());
    }

    const std::vector<std::pair<std::string, index_family>> term{
        {"libor", index_family::libor},
        {"euribor", index_family::euribor},
        {"shibor", index_family::shibor},
        {"tiie", index_family::tiie},
        {"taibor", index_family::taibor},
        {"bbsw", index_family::bbsw},
        {"cibor", index_family::cibor},
        {"cms", index_family::cms},
        {"hibor", index_family::hibor},
        {"nibor", index_family::nibor},
        {"pribor", index_family::pribor},
        {"repofix", index_family::repofix},
        {"stibor", index_family::stibor},
    };
    REQUIRE_FALSE(term.empty());
    for (const auto& [name, expected] : term) {
        REQUIRE_THROWS_AS(
            oresmd_parser::parse(uri("oresmd://ir/usd?index=" + name + "&type=fixing")),
            oresmd_exception);
        const auto id =
            oresmd_parser::parse(uri("oresmd://ir/usd?index=" + name + "&tenor=3m&type=fixing"));
        const auto& ir = std::get<ir_market_data_identifier>(id);
        REQUIRE(ir.index == expected);
        REQUIRE(ir.tenor.has_value());
    }
}

TEST_CASE("parse_ir_swap_quote", tags) {
    const auto id = oresmd_parser::parse(uri(
        "oresmd://ir/"
        "usd?index=libor&tenor=3m&role=projection&type=quote&quote=ir_swap&metric=rate&point=5y"));
    const auto& ir = std::get<ir_market_data_identifier>(id);
    REQUIRE(ir.type == instrument_type::quote);
    REQUIRE(ir.quote_type == ir_quote_type::ir_swap);
    REQUIRE(ir.metric == metric::rate);
    REQUIRE(ir.point == "5y");
}

TEST_CASE("parse_discount_quote", tags) {
    const auto id = oresmd_parser::parse(uri("oresmd://ir/usd?index=libor&tenor=3m&role=projection&"
                                             "type=quote&quote=discount&metric=rate&point=6m"));
    const auto& ir = std::get<ir_market_data_identifier>(id);
    REQUIRE(ir.quote_type == ir_quote_type::discount);
    REQUIRE(ir.metric == metric::rate);
    REQUIRE(ir.point == "6m");
}

TEST_CASE("parse_swaption_vol_populates_tenor_and_vol_struct", tags) {
    const auto id = oresmd_parser::parse(uri("oresmd://ir/eur?type=vol&point=5y,2y,atm"));
    const auto& ir = std::get<ir_market_data_identifier>(id);
    REQUIRE(ir.ccy == "EUR");
    REQUIRE(ir.type == instrument_type::vol);
    REQUIRE_FALSE(ir.index.has_value());
    REQUIRE(ir.tenor == "2y");
    REQUIRE_FALSE(ir.role.has_value());
    REQUIRE(ir.vol.has_value());
    REQUIRE(ir.vol->expiry == "5Y");
    REQUIRE(ir.vol->strike == "ATM");
}

TEST_CASE("parse_equity_quote", tags) {
    const auto id = oresmd_parser::parse(uri("oresmd://equity/aapl?ccy=usd&type=quote"));
    const auto& eq = std::get<equity_market_data_identifier>(id);
    REQUIRE(eq.ticker == "AAPL");
    REQUIRE(eq.ccy == "USD");
}

TEST_CASE("parse_credit_cds_quote", tags) {
    const auto id = oresmd_parser::parse(
        uri("oresmd://credit/itraxx-europe?ccy=eur&type=quote&quote=cds&point=sr,5y"));
    const auto& cr = std::get<credit_market_data_identifier>(id);
    REQUIRE(cr.reference_entity == "ITRAXX-EUROPE");
    REQUIRE(cr.ccy == "EUR");
    REQUIRE(cr.quote_type == credit_quote_type::cds);
    REQUIRE(cr.point == "sr,5y");
}

TEST_CASE("parse_credit_hazard_rate", tags) {
    const auto id = oresmd_parser::parse(
        uri("oresmd://credit/vod?ccy=eur&type=quote&quote=hazard_rate&point=sr,5y"));
    const auto& cr = std::get<credit_market_data_identifier>(id);
    REQUIRE(cr.quote_type == credit_quote_type::hazard_rate);
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
    const auto original = oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote&quote=spot"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact, whether a quote key or an index
    // name. A documented example that parses and round-trips while projecting to
    // nothing is the defect this case exists to catch, and the stability check
    // above passes for it either way. Whether a name reads back exactly is the
    // corpus-driven coverage tests' claim, not this one's.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_fx_fwd", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote&quote=fwd&point=6m"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact, whether a quote key or an index
    // name. A documented example that parses and round-trips while projecting to
    // nothing is the defect this case exists to catch, and the stability check
    // above passes for it either way. Whether a name reads back exactly is the
    // corpus-driven coverage tests' claim, not this one's.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_fx_option_vol", tags) {
    const auto original = oresmd_parser::parse(uri("oresmd://fx/eurusd?type=vol&point=10y,atm"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact, whether a quote key or an index
    // name. A documented example that parses and round-trips while projecting to
    // nothing is the defect this case exists to catch, and the stability check
    // above passes for it either way. Whether a name reads back exactly is the
    // corpus-driven coverage tests' claim, not this one's.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_fx_fixing_source", tags) {
    const auto original = oresmd_parser::parse(uri("oresmd://fx/eurusd?type=fixing&source=ecb"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact, whether a quote key or an index
    // name. A documented example that parses and round-trips while projecting to
    // nothing is the defect this case exists to catch, and the stability check
    // above passes for it either way. Whether a name reads back exactly is the
    // corpus-driven coverage tests' claim, not this one's.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_fx_fixing_source_spelling", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://fx/usdeur?type=fixing&source=reuters&source_spelling=Reuters"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact, whether a quote key or an index
    // name. A documented example that parses and round-trips while projecting to
    // nothing is the defect this case exists to catch, and the stability check
    // above passes for it either way. Whether a name reads back exactly is the
    // corpus-driven coverage tests' claim, not this one's.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_fx_fixing_reversed_pair", tags) {
    const auto original = oresmd_parser::parse(uri("oresmd://fx/gbpeur?type=fixing&source=tr20h"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact, whether a quote key or an index
    // name. A documented example that parses and round-trips while projecting to
    // nothing is the defect this case exists to catch, and the stability check
    // above passes for it either way. Whether a name reads back exactly is the
    // corpus-driven coverage tests' claim, not this one's.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_quote", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/usd?tenor=3m&type=quote&quote=ir_swap&metric=rate&point=5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_capfloor_normal_vol", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/chf?type=vol&quote=capfloor&model=rate_nvol&point=5y,6m,0,0,0.03"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_capfloor_shift", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://ir/eur?type=vol&quote=capfloor&model=shift&tenor=6m"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_bond_option_vol", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/eur_generic?type=vol&quote=bond_option&point=1y,10y,atm"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_equity_fixing", tags) {
    const auto original = oresmd_parser::parse(uri("oresmd://equity/sp5?type=fixing"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_equity_fixing_with_an_identifier_scheme", tags) {
    const auto original = oresmd_parser::parse(uri("oresmd://equity/ric:.spx?type=fixing"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_equity", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://equity/aapl?ccy=usd&type=quote&quote=spot"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_equity_dividend", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://equity/aapl?ccy=usd&type=quote&quote=dividend&point=1y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_equity_fwd", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://equity/lufthansa?ccy=eur&type=quote&quote=fwd&point=6m"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_equity_option_vol", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://equity/sp5?ccy=usd&type=vol&point=6m,atmf"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_equity_option_price", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://equity/ric:.stoxx50e?ccy=eur&type=vol&point=2021-07-16,1200,c&model=price"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_equity_option_delta", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://equity/ric:.spx?ccy=usd&type=vol&point=1080d,del,spot,call,0.1"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_credit", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://credit/itraxx-europe?ccy=eur&type=quote&quote=cds&point=sr,5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_hazard_rate", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://credit/vod?ccy=eur&type=quote&quote=hazard_rate&point=sr,5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_recovery_rate", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://credit/vod?ccy=eur&type=quote&quote=recovery_rate&point=sr"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_cds_index", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://credit/cdx-na-ig?ccy=usd&type=quote&quote=cds_index&point=5y,0.1"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_index_cds_tranche", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://credit/2i65byeg6?ccy=usd&type=quote&quote=index_cds_tranche&point=5y,0.07"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_cds_with_restructuring_clause", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://credit/025adx?ccy=usd&type=quote&quote=cds&point=snrfor,xr14,10y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_recovery_rate_with_restructuring_clause", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://credit/025adx?ccy=usd&type=quote&quote=recovery_rate&point=snrfor,mr14"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_index_cds_option_vol", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://credit/"
                                 "2i65byeg6?ccy=usd&type=vol&quote=index_cds_option&model=rate_"
                                 "lnvol&point=5y,2025-02-19,107.5"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_index_cds_option_term_vol", tags) {
    const auto original = oresmd_parser::parse(uri(
        "oresmd://credit/cdxig?ccy=usd&type=vol&quote=index_cds_option&model=rate_lnvol&point=1m"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_commodity", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://commodity/gold?ccy=usd&type=quote&quote=spot"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_commodity_fwd", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://commodity/wti?ccy=usd&type=quote&quote=fwd&point=6m"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_commodity_option_delta_vol", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://commodity/"
            "ice:b?ccy=usd&type=vol&quote=option&model=rate_lnvol&point=10m,del,fwd,call,0.10"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_commodity_fixing_delivery", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://commodity/ice:b?type=fixing&delivery=2024-12"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

/*
 * The fixing-only classes: an intraday power index and a generic index are named
 * by their index names and by nothing else, so each URI below projects an index
 * name and no quote key.
 */

TEST_CASE("round_trip_power", tags) {
    const auto original = oresmd_parser::parse(uri("oresmd://power/ice:pdq?type=fixing"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_power_delivery_date", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://power/ice:pdq?type=fixing&delivery=2021-01-04"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_power_delivery_window", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://power/ice:pdq?type=fixing&delivery=2021-01-04-14400-18000"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_power_delivery_window_dst", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://power/ice:pdq?type=fixing&delivery=2021-01-04-14400-18000-dst"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_generic", tags) {
    const auto original = oresmd_parser::parse(uri("oresmd://generic/juniornote?type=fixing"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_generic_with_a_spelled_name", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://generic/juniornote?type=fixing&name_spelling=JuniorNote"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_generic_name_with_a_key_separator", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://generic/portfoliobasket%231?type=fixing"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

/*
 * Round-trip tests for new IR quote types (id:D566131C-D08C-4AFE-950E-B3DD26EB2C24).
 */

TEST_CASE("round_trip_ir_mm_rate", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=mm&metric=rate&point=1m"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_mm_rate_without_index", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/usd?tenor=2d&type=quote&quote=mm&metric=rate&point=3m"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_fra_rate", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/usd?tenor=1m&type=quote&quote=fra&metric=rate&point=3m"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_imm_fra_rate", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/nok?tenor=1&type=quote&quote=imm_fra&metric=rate&point=2"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_basis_swap_spread", tags) {
    const auto original = oresmd_parser::parse(uri(
        "oresmd://ir/"
        "eur?tenor=3m&second_tenor=6m&type=quote&quote=basis_swap&metric=basis_spread&point=10y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_cc_basis_swap", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://ir/"
                                 "usd?tenor=3m&second_tenor=3m&second_ccy=eur&type=quote&quote=cc_"
                                 "basis_swap&metric=basis_spread&point=10y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_mm_future_price", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://ir/"
                                 "eur?tenor=3m&contract_month=2024-04&contract_code=XICE:FEI&type="
                                 "quote&quote=mm_future&metric=price"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_zero_yield_spread", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://ir/"
                                 "eur?type=quote&quote=zero&metric=yield_spread&curve_id=EONIA_"
                                 "ESTER_SPREAD&day_count=A365&point=1d"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_oi_future_price", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://ir/"
                                 "usd?tenor=3m&contract_month=2024-01&contract_code=XCME:SRA&type="
                                 "quote&quote=oi_future&metric=price"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_discount", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/usd?curve_id=USD3M&tenor=6m&type=quote&quote=discount&metric=rate"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_discount_currency_named_curve", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/eur?curve_id=EUR&tenor=1d&type=quote&quote=discount&metric=rate"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_swap_indexed", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/"
            "usd?index=sofr&settle=0D&tenor=1d&type=quote&quote=ir_swap&metric=rate&point=2y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_mm_index_spelling", tags) {
    const auto original = oresmd_parser::parse(uri(
        "oresmd://ir/"
        "eur?index=estr&index_spelling=ESTER&tenor=0d&type=quote&quote=mm&metric=rate&point=1d"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_swaption_indexed", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/usd?index=sofr&type=vol&model=rate_nvol&point=10y,10y,ATM"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_swaption_smile", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/eur?type=vol&model=rate_nvol&point=6m,2y,Smile,-0.02"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_basis_swap_named_form", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://ir/"
                                 "usd?index_spelling=SOFR_FedFunds&tenor=1d&second_tenor=1d&type="
                                 "quote&quote=basis_swap&metric=basis_spread&point=10y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_bma_swap_ratio", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/usd?tenor=3m&type=quote&quote=bma_swap&metric=ratio&point=5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_ir_cc_fix_float_swap", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://ir/"
                                 "usd?tenor=3m&second_tenor=1y&second_ccy=try&type=quote&quote=cc_"
                                 "fix_float_swap&metric=rate&point=1y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
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

TEST_CASE("reject_fx_model_when_type_not_vol", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote&model=rate_lnvol")),
                      oresmd_exception);
}

TEST_CASE("reject_ir_uri_with_ccy_query_key_since_entity_already_is_the_currency", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://ir/usd?type=fixing&index=sofr&ccy=usd")),
                      oresmd_exception);
}

TEST_CASE("reject_ir_metric_present_when_type_is_not_quote", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://ir/usd?index=libor&tenor=3m&type=fixing&metric=rate")),
        oresmd_exception);
}

TEST_CASE("reject_ir_model_when_type_not_vol", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://ir/usd?type=quote&model=rate_lnvol")),
                      oresmd_exception);
}

TEST_CASE("reject_ir_entity_that_is_not_a_currency", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://ir/notaccy?type=quote")),
                      oresmd_exception);
}

TEST_CASE("reject_ir_bond_option_ccy_query_key", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(
            uri("oresmd://ir/eur_generic?type=vol&quote=bond_option&point=1y,10y,atm&ccy=eur")),
        oresmd_exception);
}

TEST_CASE("parse_ir_metric_present_when_type_is_omitted_defaults_to_quote", tags) {
    const auto id = oresmd_parser::parse(uri("oresmd://ir/usd?index=libor&tenor=3m&metric=rate"));
    const auto& ir = std::get<ir_market_data_identifier>(id);
    REQUIRE(ir.type == instrument_type::quote);
    REQUIRE(ir.metric == metric::rate);
}

TEST_CASE("parse_equity_dividend_quote", tags) {
    const auto id =
        oresmd_parser::parse(uri("oresmd://equity/aapl?ccy=usd&type=quote&quote=dividend"));
    const auto& eq = std::get<equity_market_data_identifier>(id);
    REQUIRE(eq.quote_type == equity_quote_type::dividend);
}

TEST_CASE("reject_equity_uri_with_ir_only_tenor_field", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://equity/aapl?ccy=usd&type=quote&tenor=3m")),
                      oresmd_exception);
}

TEST_CASE("parse_equity_with_point", tags) {
    const auto id = oresmd_parser::parse(
        uri("oresmd://equity/aapl?ccy=usd&type=quote&quote=dividend&point=1y"));
    const auto& eq = std::get<equity_market_data_identifier>(id);
    REQUIRE(eq.quote_type == equity_quote_type::dividend);
    REQUIRE(eq.point == "1y");
}

TEST_CASE("reject_equity_quote_when_type_not_quote", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://equity/aapl?ccy=usd&type=fixing&quote=dividend")),
        oresmd_exception);
}

TEST_CASE("reject_equity_uri_missing_mandatory_ccy", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://equity/aapl?type=quote")),
                      oresmd_exception);
}

TEST_CASE("reject_equity_model_when_type_not_vol", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://equity/aapl?ccy=usd&type=quote&model=rate_lnvol")),
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

TEST_CASE("reject_credit_quote_when_type_not_quote", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://credit/itraxx-europe?ccy=eur&type=curve&quote=cds")),
        oresmd_exception);
}

TEST_CASE("reject_credit_model_when_type_not_vol", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(
                          uri("oresmd://credit/itraxx-europe?ccy=eur&type=quote&model=rate_lnvol")),
                      oresmd_exception);
}

TEST_CASE("parse_correlation_pairwise", tags) {
    const auto id =
        oresmd_parser::parse(uri("oresmd://correlation/ccy-eur-usd?type=quote&quote=pairwise"));
    const auto& cr = std::get<correlation_market_data_identifier>(id);
    REQUIRE(cr.factor_pair == "CCY-EUR-USD");
    REQUIRE(cr.quote_type == correlation_quote_type::pairwise);
}

TEST_CASE("round_trip_correlation", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://correlation/ccy-eur-usd?type=quote&quote=pairwise"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_correlation_surface", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://correlation/"
                                 "fx-generic-gbp-usd?type=quote&quote=pairwise&second_factor=fx-"
                                 "generic-eur-usd&point=1y,atm"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_security_fixing", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://security/isin:ie00bh3sq895?type=fixing"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_security_fixing_delivery", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://security/isin:ie00bh3sq895?type=fixing&delivery=2025-08"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_security_bond_price", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://security/isin:de000a3h2wp2?type=quote&quote=bond_price"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_security_bond_yield_spread", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://security/security_1?type=quote&quote=bond_yield_spread"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_security_bond_conversion_factor", tags) {
    const auto original = oresmd_parser::parse(uri(
        "oresmd://security/security_1_fwdexp_20251220?type=quote&quote=bond_conversion_factor"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_security_recovery_rate", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://security/security_1?type=quote&quote=recovery_rate"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_security_cpr", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://security/isin:xs0983610930?type=quote&quote=cpr"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_shape_profile_factor", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://shape_profile/"
            "pjm_wh_rt_pk?type=quote&quote=shape_factor&point=2021-03-01,0,sec"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_shape_profile_factor_dst", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://shape_profile/"
            "pjm_wh_rt_pk_15min?type=quote&quote=shape_factor&point=2021-03-01,10800,sec,dst"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_rating_transition_probability", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://rating/provider_1?type=quote&quote=transition_probability&point=aaa,aa"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_rating_transition_probability_without_grades", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://rating/provider_1?type=quote&quote=transition_probability&point="));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("parse_inflation_zc_swap", tags) {
    const auto id =
        oresmd_parser::parse(uri("oresmd://inflation/ukrpi?type=quote&quote=zc_swap&point=5y"));
    const auto& inf = std::get<inflation_market_data_identifier>(id);
    REQUIRE(inf.index_code == "UKRPI");
    REQUIRE(inf.quote_type == inflation_quote_type::zc_swap);
    REQUIRE(inf.point == "5y");
}

TEST_CASE("round_trip_inflation_fixing", tags) {
    const auto original = oresmd_parser::parse(uri("oresmd://inflation/ukrpi?type=fixing"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_inflation", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://inflation/ukrpi?type=quote&quote=zc_swap&point=5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_inflation_yy_swap", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://inflation/ukrpi?type=quote&quote=yy_swap&point=5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_inflation_seasonality", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://inflation/ukrpi?type=quote&quote=seasonality&point=jan"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_inflation_zc_capfloor_price", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://inflation/euhicpxt?type=vol&quote=zc_capfloor&model=price&point=10y,c,0.00"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_inflation_cf_price", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://inflation/euhicpxt?type=vol&quote=cf_price&model=price&point=10y,c,0.00"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("round_trip_inflation_yy_capfloor_normal_vol", tags) {
    const auto original = oresmd_parser::parse(uri(
        "oresmd://inflation/euhicpxt?type=vol&quote=yy_capfloor&model=rate_nvol&point=5y,f,0.02"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
    // The URI must also name a real ORE artefact. A documented example that
    // parses and round-trips while projecting to nothing is the defect this case
    // exists to catch, and the stability check above passes for it either way.
    // Deliberately no key-readback comparison: a URI may legitimately carry
    // context the key does not encode (a quote's curve index and role), and the
    // key-to-URI direction normalises the entity's case, which the corpus-driven
    // coverage tests own.
    REQUIRE((oresmd_projections::to_quote_key(original).has_value() ||
             oresmd_projections::to_index_name(original).has_value() ||
             oresmd_projections::to_curve_key(original).has_value()));
}

TEST_CASE("parse_commodity_fwd_quote", tags) {
    const auto id =
        oresmd_parser::parse(uri("oresmd://commodity/gold?ccy=usd&type=quote&quote=fwd&point=6m"));
    const auto& co = std::get<commodity_market_data_identifier>(id);
    REQUIRE(co.quote_type == commodity_quote_type::fwd);
    REQUIRE(co.point == "6m");
}

TEST_CASE("parse_generic_index", tags) {
    // The path segment is upper-cased like every other class's entity, and the
    // spelling the corpus wrote is carried raw beside it, so the index name the
    // fixing boundary writes back is the one ORE wrote.
    const auto id = oresmd_parser::parse(
        uri("oresmd://generic/juniornote?type=fixing&name_spelling=JuniorNote"));
    const auto& generic = std::get<generic_market_data_identifier>(id);
    CHECK(generic.type == instrument_type::fixing);
    CHECK(generic.name == "JUNIORNOTE");
    REQUIRE(generic.name_spelling.has_value());
    CHECK(*generic.name_spelling == "JuniorNote");
}

TEST_CASE("reject_commodity_quote_when_type_not_quote", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://commodity/gold?ccy=usd&type=fixing&quote=fwd")),
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

TEST_CASE("reject_commodity_model_when_type_not_vol", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://commodity/gold?ccy=usd&type=quote&model=rate_lnvol")),
        oresmd_exception);
}

TEST_CASE("reject_commodity_delivery_on_a_quote", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(
                          uri("oresmd://commodity/"
                              "ice:b?ccy=usd&type=quote&quote=fwd&point=2024-12&delivery=2024-12")),
                      oresmd_exception);
}

TEST_CASE("reject_power_uri_with_a_currency", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://power/ice:pdq?type=fixing&ccy=usd")),
                      oresmd_exception);
}

TEST_CASE("reject_power_uri_with_a_point", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://power/ice:pdq?type=fixing&point=2021-01-04")),
        oresmd_exception);
}

TEST_CASE("reject_power_uri_with_a_quote_type", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://power/ice:pdq?type=fixing&quote=spot")),
                      oresmd_exception);
}

TEST_CASE("reject_power_uri_with_a_source", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://power/ice:pdq?type=fixing&source=ecb")),
                      oresmd_exception);
}

TEST_CASE("reject_power_uri_with_a_tenor", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://power/ice:pdq?type=fixing&tenor=3m")),
                      oresmd_exception);
}

TEST_CASE("reject_power_uri_with_an_index", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://power/ice:pdq?type=fixing&index=libor")),
                      oresmd_exception);
}

TEST_CASE("reject_power_delivery_on_a_quote", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://power/ice:pdq?type=quote&delivery=2021-01-04")),
        oresmd_exception);
}

TEST_CASE("reject_generic_uri_with_a_currency", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://generic/juniornote?type=fixing&ccy=usd")),
                      oresmd_exception);
}

TEST_CASE("reject_generic_uri_with_an_index", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://generic/juniornote?type=fixing&index=libor")),
        oresmd_exception);
}

TEST_CASE("reject_generic_uri_with_a_tenor", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://generic/juniornote?type=fixing&tenor=3m")),
                      oresmd_exception);
}

TEST_CASE("reject_generic_uri_with_a_delivery", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://generic/juniornote?type=fixing&delivery=2025-10")),
        oresmd_exception);
}

TEST_CASE("reject_generic_uri_with_a_quote_type", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://generic/juniornote?type=fixing&quote=spot")),
        oresmd_exception);
}

TEST_CASE("reject_generic_uri_with_a_point", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://generic/juniornote?type=fixing&point=2025-10-03")),
        oresmd_exception);
}

TEST_CASE("reject_generic_uri_with_a_source", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://generic/juniornote?type=fixing&source=ecb")),
        oresmd_exception);
}

TEST_CASE("reject_generic_uri_whose_spelling_is_not_a_case_of_the_name", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri(
                          "oresmd://generic/juniornote?type=fixing&name_spelling=NotJuniorNote")),
                      oresmd_exception);
}

TEST_CASE("reject_security_uri_with_ccy", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(
                          uri("oresmd://security/security_1?ccy=usd&type=quote&quote=bond_price")),
                      oresmd_exception);
}

TEST_CASE("reject_security_uri_with_ir_only_metric", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri(
                          "oresmd://security/security_1?type=quote&quote=bond_price&metric=price")),
                      oresmd_exception);
}

TEST_CASE("reject_security_model_query_key", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(
            uri("oresmd://security/security_1?type=quote&quote=bond_price&model=rate_lnvol")),
        oresmd_exception);
}

TEST_CASE("reject_security_delivery_on_a_quote", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri(
            "oresmd://security/isin:ie00bh3sq895?type=quote&quote=bond_price&delivery=2025-08")),
        oresmd_exception);
}

TEST_CASE("reject_shape_profile_uri_with_ccy", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(
            uri("oresmd://shape_profile/"
                "pjm_wh_rt_pk?type=quote&quote=shape_factor&point=2021-03-01,0,sec&ccy=usd")),
        oresmd_exception);
}

TEST_CASE("reject_shape_profile_uri_with_ir_only_metric", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(
            uri("oresmd://shape_profile/pjm_wh_rt_pk?type=quote&quote=shape_factor&metric=rate")),
        oresmd_exception);
}

TEST_CASE("reject_shape_profile_model_query_key", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri(
            "oresmd://shape_profile/"
            "pjm_wh_rt_pk?type=quote&quote=shape_factor&point=2021-03-01,0,sec&model=rate_lnvol")),
        oresmd_exception);
}

TEST_CASE("reject_rating_uri_with_ccy", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(
            uri("oresmd://rating/"
                "provider_1?ccy=usd&type=quote&quote=transition_probability&point=aaa,aa")),
        oresmd_exception);
}

TEST_CASE("reject_rating_uri_with_ir_only_metric", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(
            uri("oresmd://rating/"
                "provider_1?type=quote&quote=transition_probability&point=aaa,aa&metric=rate")),
        oresmd_exception);
}

TEST_CASE("reject_rating_model_query_key", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri(
            "oresmd://rating/"
            "provider_1?type=quote&quote=transition_probability&point=aaa,aa&model=rate_lnvol")),
        oresmd_exception);
}

TEST_CASE("reject_correlation_model_query_key", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(
            uri("oresmd://correlation/ccy-eur-usd?type=quote&quote=pairwise&model=rate_lnvol")),
        oresmd_exception);
}

TEST_CASE("reject_inflation_model_when_type_not_vol", tags) {
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(
            uri("oresmd://inflation/ukrpi?type=quote&quote=zc_swap&point=5y&model=rate_lnvol")),
        oresmd_exception);
}

TEST_CASE("reject_inflation_fixing_code_the_model_does_not_carry", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://inflation/notacode?type=fixing")),
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

TEST_CASE("reject_unrecognised_query_key", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://ir/usd?index=libor&tenr=3m&type=fixing")),
                      oresmd_exception);
}

TEST_CASE("reject_fx_quote_when_type_not_quote", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://fx/eurusd?type=fixing&quote=spot")),
                      oresmd_exception);
}

TEST_CASE("reject_fx_entity_that_is_not_a_six_letter_pair", tags) {
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://fx/eur?type=quote")), oresmd_exception);
}

TEST_CASE("parse_fx_fixing_carries_the_source_it_was_published_by", tags) {
    // A fixing's source is part of its identity: ECB and TR20H both fix EUR-USD
    // and they are different rates, so one pair is not one series.
    const auto id =
        oresmd_parser::parse(uri("oresmd://fx/eurusd?type=fixing&source=ecb&source_spelling=ECB"));
    const auto& fx = std::get<fx_market_data_identifier>(id);
    REQUIRE(fx.type == instrument_type::fixing);
    REQUIRE(fx.source.has_value());
    CHECK(*fx.source == "ecb");
    REQUIRE(fx.source_spelling.has_value());
    CHECK(*fx.source_spelling == "ECB");
    // The pair is the pair as published, never a canonical order.
    CHECK(fx.pair == "EURUSD");
}

TEST_CASE("parse_fx_lower_cases_the_source_but_not_its_spelling", tags) {
    const auto id = oresmd_parser::parse(uri("oresmd://fx/usdeur?type=fixing&source=Reuters"));
    const auto& fx = std::get<fx_market_data_identifier>(id);
    REQUIRE(fx.source.has_value());
    CHECK(*fx.source == "reuters");
    CHECK(fx.pair == "USDEUR");
}

TEST_CASE("reject_fx_source_when_type_is_not_fixing", tags) {
    // ORE's FX quote keys carry no source at all: the token appears in index
    // names alone, so a sourced quote names nothing the corpus writes.
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote&source=ecb")),
                      oresmd_exception);
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote&source_spelling=ECB")),
        oresmd_exception);
    // A fixing that names no source is still a fixing; the field is optional.
    const auto id = oresmd_parser::parse(uri("oresmd://fx/eurusd?type=fixing"));
    CHECK_FALSE(std::get<fx_market_data_identifier>(id).source.has_value());
}

TEST_CASE("reject_fx_source_keys_on_every_other_asset_class", tags) {
    // The source keys name an FX fixing and nothing else. correlation, inflation,
    // ir, rating, security, shape_profile and commodity refuse them through the
    // Reject keys table their model carries; equity and credit carry such a table
    // too but generate only the shared check, so the shared check is what refuses
    // them there. Every class but FX is here.
    for (const auto& rejected : {"oresmd://equity/sp5?type=fixing&source=ecb",
                                 "oresmd://credit/vod?ccy=eur&type=fixing&source=ecb",
                                 "oresmd://commodity/gold?ccy=usd&type=fixing&source=ecb",
                                 "oresmd://inflation/ukrpi?type=fixing&source=ecb",
                                 "oresmd://ir/usd?type=fixing&index=sofr&source=ecb",
                                 "oresmd://correlation/ccy-eur-usd?type=fixing&source=ecb",
                                 "oresmd://rating/provider_1?type=fixing&source=ecb",
                                 "oresmd://security/isin:de000a3h2wp2?type=fixing&source=ecb",
                                 "oresmd://shape_profile/pjm_wh_rt_pk?type=fixing&source=ecb",
                                 "oresmd://equity/sp5?type=fixing&source_spelling=ECB"}) {
        REQUIRE_THROWS_AS(oresmd_parser::parse(uri(rejected)), oresmd_exception);
    }
}

TEST_CASE("reject_delivery_on_every_class_that_does_not_name_it", tags) {
    // A delivery coordinate is a fixing's: a commodity future's contract month, a
    // bond future's expiry month or an intraday power index's delivery window. A key
    // accepted and dropped is a key the caller believes was recorded, so every class
    // but those three refuses it anywhere, through its own Reject keys table or the
    // shared check, and those three refuse it off a fixing, which their own Rejection
    // tables declare.
    for (const auto& rejected :
         {"oresmd://equity/sp5?type=fixing&delivery=2024-12",
          "oresmd://credit/vod?ccy=eur&type=fixing&delivery=2024-12",
          "oresmd://fx/eurusd?type=fixing&source=ecb&delivery=2024-12",
          "oresmd://inflation/ukrpi?type=fixing&delivery=2024-12",
          "oresmd://ir/usd?type=fixing&index=sofr&delivery=2024-12",
          "oresmd://correlation/ccy-eur-usd?type=fixing&delivery=2024-12",
          "oresmd://rating/provider_1?type=fixing&delivery=2024-12",
          "oresmd://generic/juniornote?type=fixing&delivery=2024-12",
          "oresmd://shape_profile/pjm_wh_rt_pk?type=fixing&delivery=2024-12"}) {
        REQUIRE_THROWS_AS(oresmd_parser::parse(uri(rejected)), oresmd_exception);
    }
    // The three classes that name it keep it on a fixing.
    const auto commodity = std::get<commodity_market_data_identifier>(
        oresmd_parser::parse(uri("oresmd://commodity/ice:b?type=fixing&delivery=2024-12")));
    REQUIRE(commodity.delivery.has_value());
    CHECK(*commodity.delivery == "2024-12");
    const auto power = std::get<power_market_data_identifier>(
        oresmd_parser::parse(uri("oresmd://power/ice:pdq?type=fixing&delivery=2024-12")));
    REQUIRE(power.delivery.has_value());
    CHECK(*power.delivery == "2024-12");
    const auto security = std::get<security_market_data_identifier>(oresmd_parser::parse(
        uri("oresmd://security/isin:ie00bh3sq895?type=fixing&delivery=2025-08")));
    REQUIRE(security.delivery.has_value());
    CHECK(*security.delivery == "2025-08");
}

TEST_CASE("reject_name_spelling_on_every_class_that_does_not_name_it", tags) {
    // A generic index's spelling carries the case ORE wrote the index name in, and
    // nothing else. Every class but generic refuses it, through its own Reject keys
    // table or, where the model generates none, through the shared check.
    for (const auto& rejected :
         {"oresmd://equity/sp5?type=fixing&name_spelling=Sp5",
          "oresmd://credit/vod?ccy=eur&type=fixing&name_spelling=Vod",
          "oresmd://commodity/ice:b?type=fixing&name_spelling=B",
          "oresmd://power/ice:pdq?type=fixing&name_spelling=Pdq",
          "oresmd://fx/eurusd?type=fixing&source=ecb&name_spelling=Ecb",
          "oresmd://inflation/ukrpi?type=fixing&name_spelling=Ukrpi",
          "oresmd://ir/usd?type=fixing&index=sofr&name_spelling=Sofr",
          "oresmd://correlation/ccy-eur-usd?type=fixing&name_spelling=Ccy",
          "oresmd://rating/provider_1?type=fixing&name_spelling=Provider",
          "oresmd://security/isin:de000a3h2wp2?type=fixing&name_spelling=Isin",
          "oresmd://shape_profile/pjm_wh_rt_pk?type=fixing&name_spelling=Pjm"}) {
        REQUIRE_THROWS_AS(oresmd_parser::parse(uri(rejected)), oresmd_exception);
    }
    // The class that names it keeps it raw: the spelling is the arrival text, so it
    // is carried in the case it arrived in.
    const auto generic = std::get<generic_market_data_identifier>(oresmd_parser::parse(
        uri("oresmd://generic/juniornote?type=fixing&name_spelling=JuniorNote")));
    REQUIRE(generic.name_spelling.has_value());
    CHECK(*generic.name_spelling == "JuniorNote");
}

TEST_CASE("reject_ir_term_index_fixing_without_a_tenor", tags) {
    // A term index (libor/euribor) needs a tenor to disambiguate which point on the
    // curve it fixes at; an overnight index (sofr/estr/...) does not have this
    // requirement -- see parse_ir_usd_sofr_discount_fixing above, which omits it too
    // (its tenor is supplied for the curve key, but the check below is term-index-only).
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://ir/usd?index=libor&type=fixing")),
                      oresmd_exception);
}

/*
 * Canonical form contract (acceptance d): to_uri(parse(uri)) == uri for URIs already
 * in canonical form — the fixed per-asset-class parameter order, lowercased
 * components, absent fields skipped. This pins the canonical string equality lookup
 * compares against.
 */

TEST_CASE("canonical_string_round_trip_fx_spot", tags) {
    const auto s = uri("oresmd://fx/eurusd?type=quote&quote=spot");
    REQUIRE(oresmd_parser::to_uri(oresmd_parser::parse(s)).value == s.value);
}

TEST_CASE("canonical_string_round_trip_fx_fwd", tags) {
    const auto s = uri("oresmd://fx/eurusd?type=quote&quote=fwd&point=6m");
    REQUIRE(oresmd_parser::to_uri(oresmd_parser::parse(s)).value == s.value);
}

TEST_CASE("canonical_string_round_trip_ir_fixing", tags) {
    const auto s = uri("oresmd://ir/usd?index=libor&tenor=3m&role=projection&type=fixing");
    REQUIRE(oresmd_parser::to_uri(oresmd_parser::parse(s)).value == s.value);
}

TEST_CASE("canonical_string_round_trip_ir_quote", tags) {
    const auto s = uri(
        "oresmd://ir/"
        "usd?index=libor&tenor=3m&role=projection&type=quote&metric=rate&quote=ir_swap&point=5y");
    REQUIRE(oresmd_parser::to_uri(oresmd_parser::parse(s)).value == s.value);
}

TEST_CASE("canonical_string_round_trip_ir_vol", tags) {
    const auto s = uri("oresmd://ir/eur?tenor=2y&type=vol&point=5y,2y,atm");
    REQUIRE(oresmd_parser::to_uri(oresmd_parser::parse(s)).value == s.value);
}

TEST_CASE("canonical_string_round_trip_equity", tags) {
    const auto s = uri("oresmd://equity/aapl?ccy=usd&type=quote&quote=dividend&point=1y");
    REQUIRE(oresmd_parser::to_uri(oresmd_parser::parse(s)).value == s.value);
}

TEST_CASE("canonical_string_round_trip_credit", tags) {
    const auto s = uri("oresmd://credit/itraxx-europe?ccy=eur&type=quote&quote=cds&point=sr,5y");
    REQUIRE(oresmd_parser::to_uri(oresmd_parser::parse(s)).value == s.value);
}

TEST_CASE("canonical_string_round_trip_commodity", tags) {
    const auto s = uri("oresmd://commodity/wti?ccy=usd&type=quote&quote=fwd&point=6m");
    REQUIRE(oresmd_parser::to_uri(oresmd_parser::parse(s)).value == s.value);
}

TEST_CASE("canonical_string_round_trip_inflation", tags) {
    const auto s = uri("oresmd://inflation/ukrpi?type=quote&quote=zc_swap&point=5y");
    REQUIRE(oresmd_parser::to_uri(oresmd_parser::parse(s)).value == s.value);
}

TEST_CASE("canonical_string_round_trip_correlation", tags) {
    const auto s = uri("oresmd://correlation/ccy-eur-usd?type=quote&quote=pairwise");
    REQUIRE(oresmd_parser::to_uri(oresmd_parser::parse(s)).value == s.value);
}

TEST_CASE("stored_form_is_the_canonical_encoding_independent_of_input_spelling", tags) {
    // The stored string is exactly what to_uri() emits for the parsed identifier, never
    // the input's own spelling: the percent-encoded "%2F" and the raw "/" spellings
    // both settle on the same stored form, so matching compares like with like. (The
    // encoder leaves "/" unencoded in a query value -- both spellings normalise to it.)
    const auto encoded_input =
        uri("oresmd://ir/eur?type=quote&metric=rate&quote=ir_swap&point=5y%2F6m");
    const auto raw_input = uri("oresmd://ir/eur?type=quote&metric=rate&quote=ir_swap&point=5y/6m");
    const auto id = oresmd_parser::parse(encoded_input);
    REQUIRE(std::get<ir_market_data_identifier>(id).point == "5y/6m");
    REQUIRE(oresmd_parser::parse(raw_input) == id);
    const auto out = oresmd_parser::to_uri(id);
    REQUIRE(out.value == "oresmd://ir/eur?type=quote&metric=rate&quote=ir_swap&point=5y/6m");
    REQUIRE(oresmd_parser::parse(out) == id);
    REQUIRE(oresmd_parser::to_uri(oresmd_parser::parse(raw_input)).value == out.value);
}

/*
 * Canonical values container (acceptance e): to_uri(identifier, canonical) matches the
 * identifier's tenor and point against the supplied container and rejects unknown
 * spellings — oresmd keeps no dependency on the refdata repositories.
 */

TEST_CASE("to_uri_with_canonical_values_accepts_known_spellings", tags) {
    canonical_values cv;
    cv.tenor = {"3m", "1d"};
    cv.point = {"5y"};
    const auto id = oresmd_parser::parse(uri(
        "oresmd://ir/"
        "usd?index=libor&tenor=3m&role=projection&type=quote&metric=rate&quote=ir_swap&point=5y"));
    REQUIRE(
        oresmd_parser::to_uri(id, cv).value ==
        "oresmd://ir/"
        "usd?index=libor&tenor=3m&role=projection&type=quote&metric=rate&quote=ir_swap&point=5y");
}

TEST_CASE("to_uri_with_canonical_values_rejects_an_unknown_tenor_spelling", tags) {
    canonical_values cv;
    cv.tenor = {"6m"}; // the identifier's "3m" is not canonical
    cv.point = {"5y"};
    const auto id = oresmd_parser::parse(
        uri("oresmd://ir/usd?index=libor&tenor=3m&type=quote&quote=ir_swap&point=5y"));
    REQUIRE_THROWS_AS(oresmd_parser::to_uri(id, cv), oresmd_exception);
}

TEST_CASE("to_uri_with_canonical_values_rejects_an_unknown_point_spelling", tags) {
    canonical_values cv;
    cv.tenor = {"3m"};
    cv.point = {"6m"}; // the identifier's "5y" is not canonical
    const auto id = oresmd_parser::parse(
        uri("oresmd://ir/usd?index=libor&tenor=3m&type=quote&quote=ir_swap&point=5y"));
    REQUIRE_THROWS_AS(oresmd_parser::to_uri(id, cv), oresmd_exception);
}

TEST_CASE("to_uri_with_canonical_values_rejects_an_unknown_credit_point_spelling", tags) {
    canonical_values cv;
    cv.point = {"sr,5y"};
    const auto id =
        oresmd_parser::parse(uri("oresmd://credit/vod?ccy=eur&type=quote&quote=cds&point=sr,10y"));
    REQUIRE_THROWS_AS(oresmd_parser::to_uri(id, cv), oresmd_exception);
}

TEST_CASE("to_uri_with_canonical_values_accepts_a_vol_composite_point", tags) {
    // A type=vol identifier stores the whole composite "5y,2y,atm" as its point, so
    // the container matches the composite, not its parts; the parser also lifts the
    // composite's middle part into the identifier's tenor, and the canonical URI
    // serializes it (parse normalises the tenor-less input spelling).
    canonical_values cv;
    cv.tenor = {"2y"};
    cv.point = {"5y,2y,atm"};
    const auto id = oresmd_parser::parse(uri("oresmd://ir/eur?type=vol&point=5y,2y,atm"));
    REQUIRE(oresmd_parser::to_uri(id, cv).value ==
            "oresmd://ir/eur?tenor=2y&type=vol&point=5y,2y,atm");
}

TEST_CASE("to_uri_with_canonical_values_rejects_a_vol_point_that_is_not_the_composite", tags) {
    canonical_values cv;
    cv.tenor = {"2y"};
    cv.point = {"5y", "2y", "atm"}; // the identifier's composite "5y,2y,atm" is not canonical
    const auto id = oresmd_parser::parse(uri("oresmd://ir/eur?type=vol&point=5y,2y,atm"));
    REQUIRE_THROWS_AS(oresmd_parser::to_uri(id, cv), oresmd_exception);
}

TEST_CASE("to_uri_with_empty_canonical_values_passes_scalar_identifiers", tags) {
    // Scalars carry no tenor or point, so an empty container is fine for them.
    const canonical_values cv;
    const auto fx = oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote&quote=spot"));
    REQUIRE(oresmd_parser::to_uri(fx, cv).value == "oresmd://fx/eurusd?type=quote&quote=spot");
    const auto cr =
        oresmd_parser::parse(uri("oresmd://correlation/ccy-eur-usd?type=quote&quote=pairwise"));
    REQUIRE(oresmd_parser::to_uri(cr, cv).value ==
            "oresmd://correlation/ccy-eur-usd?type=quote&quote=pairwise");
}
/*
 * A fixing's identity carries no currency. An index name does not name the
 * currency an index is quoted in -- EQ-SP5 is the level of an index, not a
 * price in one -- so the grammar requires a ccy for every other type and not
 * for this one. The URI round trip is asserted here rather than in the
 * round-trip tables, because those also require an ORE key projection and a
 * fixing has none: ORE's equity and commodity key spaces hold quotes.
 */

TEST_CASE("parse_equity_fixing_carries_no_currency", tags) {
    const auto s = uri("oresmd://equity/sp5?type=fixing");
    const auto id = oresmd_parser::parse(s);
    const auto& eq = std::get<equity_market_data_identifier>(id);
    CHECK(eq.ticker == "SP5");
    CHECK(eq.type == instrument_type::fixing);
    CHECK_FALSE(eq.ccy.has_value());
    CHECK(oresmd_parser::to_uri(id).value == s.value);
    // And it has no ORE quote key, which is why the round-trip table cannot
    // carry it.
    CHECK_FALSE(oresmd_projections::to_quote_key(id).has_value());
}

TEST_CASE("parse_commodity_fixing_carries_no_currency", tags) {
    const auto s = uri("oresmd://commodity/gold?type=fixing");
    const auto id = oresmd_parser::parse(s);
    const auto& co = std::get<commodity_market_data_identifier>(id);
    CHECK(co.commodity_code == "GOLD");
    CHECK(co.type == instrument_type::fixing);
    CHECK_FALSE(co.ccy.has_value());
    CHECK(oresmd_parser::to_uri(id).value == s.value);
    CHECK_FALSE(oresmd_projections::to_quote_key(id).has_value());
}

TEST_CASE("parse_equity_and_commodity_still_require_a_currency_off_a_fixing", tags) {
    // The requirement moved rather than went: every type that is not a fixing
    // still has to say what the quote is denominated in.
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://equity/aapl?type=quote")),
                      oresmd_exception);
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://equity/aapl?type=vol&point=1y,atm")),
                      oresmd_exception);
    REQUIRE_THROWS_AS(oresmd_parser::parse(uri("oresmd://commodity/gold?type=quote")),
                      oresmd_exception);
}
