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
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::domain;
using ores::marketdata::core::canonical_values;
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

TEST_CASE("parse_ir_new_rfr_families_fixing_without_tenor", tags) {
    // The 16 RFR/IBOR families the SQL CHECK allows beyond the original 6
    // (synthetic_ir_curve_generation_configs_create.sql): all overnight-style, so a fixing
    // without a tenor must parse. Previously the enum lacked them, so parse_enum threw
    // "Unrecognised index value" and provisioning a party from realistic-2026 seed data failed.
    const std::vector<std::pair<std::string, index_family>> new_families{
        {"saron", index_family::saron},
        {"aonia", index_family::aonia},
        {"corra", index_family::corra},
        {"honia", index_family::honia},
        {"sora", index_family::sora},
        {"swestr", index_family::swestr},
        {"nowa", index_family::nowa},
        {"kofr", index_family::kofr},
        {"mibor", index_family::mibor},
        {"zaronia", index_family::zaronia},
        {"destr", index_family::destr},
        {"polonia", index_family::polonia},
        {"nzonia", index_family::nzonia},
        {"shibor", index_family::shibor},
        {"tiie", index_family::tiie},
        {"taibor", index_family::taibor}};
    for (const auto& [name, expected] : new_families) {
        const auto id = oresmd_parser::parse(uri("oresmd://ir/usd?index=" + name + "&type=fixing"));
        const auto& ir = std::get<ir_market_data_identifier>(id);
        REQUIRE(ir.type == instrument_type::fixing);
        REQUIRE(ir.index == expected);
        REQUIRE_FALSE(ir.tenor.has_value());
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
}

TEST_CASE("round_trip_fx_fwd", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote&quote=fwd&point=6m"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_ir_quote", tags) {
    const auto original = oresmd_parser::parse(uri(
        "oresmd://ir/"
        "usd?index=libor&tenor=3m&role=projection&type=quote&quote=ir_swap&metric=rate&point=5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_equity", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://equity/aapl?ccy=usd&type=quote&quote=spot"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_equity_dividend", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://equity/aapl?ccy=usd&type=quote&quote=dividend&point=1y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_equity_fwd", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://equity/lufthansa?ccy=eur&type=quote&quote=fwd&point=6m"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_credit", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://credit/itraxx-europe?ccy=eur&type=quote&quote=cds&point=sr,5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_hazard_rate", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://credit/vod?ccy=eur&type=quote&quote=hazard_rate&point=sr,5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_recovery_rate", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://credit/vod?ccy=eur&type=quote&quote=recovery_rate&point=sr"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_cds_index", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://credit/cdx-na-ig?ccy=usd&type=quote&quote=cds_index&point=5y,0.1"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_commodity", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://commodity/gold?ccy=usd&type=quote&quote=spot"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_commodity_fwd", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://commodity/wti?ccy=usd&type=quote&quote=fwd&point=6m"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_commodity_cpr", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://commodity/wti?ccy=usd&type=quote&quote=cpr&point=5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

/*
 * Round-trip tests for new IR quote types (id:D566131C-D08C-4AFE-950E-B3DD26EB2C24).
 */

TEST_CASE("round_trip_ir_mm_rate", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/eur?index=euribor&tenor=3m&type=quote&quote=mm&metric=rate&point=1m"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_ir_basis_swap_spread", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/"
            "eur?index=euribor&tenor=3m&type=quote&quote=basis_swap&metric=basis_spread&point=5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_ir_cc_basis_swap_no_index", tags) {
    const auto original = oresmd_parser::parse(uri(
        "oresmd://ir/eur?tenor=3m&type=quote&quote=cc_basis_swap&metric=basis_spread&point=5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_ir_mm_future_price", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/"
            "eur?index=euribor&tenor=3m&type=quote&quote=mm_future&metric=price&point=cme"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_ir_zero_yield_spread", tags) {
    const auto original = oresmd_parser::parse(
        uri("oresmd://ir/"
            "eur?index=euribor&tenor=3m&type=quote&quote=zero&metric=yield_spread&point=5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("round_trip_ir_oi_future_price", tags) {
    const auto original = oresmd_parser::parse(uri(
        "oresmd://ir/usd?index=sofr&tenor=3m&type=quote&quote=oi_future&metric=price&point=cme"));
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
    REQUIRE_THROWS_AS(
        oresmd_parser::parse(uri("oresmd://ir/usd?index=libor&tenor=3m&type=fixing&metric=rate")),
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
}

TEST_CASE("parse_inflation_zc_swap", tags) {
    const auto id =
        oresmd_parser::parse(uri("oresmd://inflation/ukrpi?type=quote&quote=zc_swap&point=5y"));
    const auto& inf = std::get<inflation_market_data_identifier>(id);
    REQUIRE(inf.index_code == "UKRPI");
    REQUIRE(inf.quote_type == inflation_quote_type::zc_swap);
    REQUIRE(inf.point == "5y");
}

TEST_CASE("round_trip_inflation", tags) {
    const auto original =
        oresmd_parser::parse(uri("oresmd://inflation/ukrpi?type=quote&quote=zc_swap&point=5y"));
    const auto roundtripped = oresmd_parser::parse(oresmd_parser::to_uri(original));
    REQUIRE(original == roundtripped);
}

TEST_CASE("parse_commodity_fwd_quote", tags) {
    const auto id =
        oresmd_parser::parse(uri("oresmd://commodity/gold?ccy=usd&type=quote&quote=fwd&point=6m"));
    const auto& co = std::get<commodity_market_data_identifier>(id);
    REQUIRE(co.quote_type == commodity_quote_type::fwd);
    REQUIRE(co.point == "6m");
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
    const auto s =
        uri("oresmd://credit/itraxx-europe?ccy=eur&type=quote&quote=cds&point=sr,5y");
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
    const auto id = oresmd_parser::parse(
        uri("oresmd://ir/"
            "usd?index=libor&tenor=3m&role=projection&type=quote&metric=rate&quote=ir_swap&point=5y"));
    REQUIRE(oresmd_parser::to_uri(id, cv).value ==
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
    const auto id = oresmd_parser::parse(
        uri("oresmd://credit/vod?ccy=eur&type=quote&quote=cds&point=sr,10y"));
    REQUIRE_THROWS_AS(oresmd_parser::to_uri(id, cv), oresmd_exception);
}

TEST_CASE("to_uri_with_empty_canonical_values_passes_scalar_identifiers", tags) {
    // Scalars carry no tenor or point, so an empty container is fine for them.
    const canonical_values cv;
    const auto fx = oresmd_parser::parse(uri("oresmd://fx/eurusd?type=quote&quote=spot"));
    REQUIRE(oresmd_parser::to_uri(fx, cv).value == "oresmd://fx/eurusd?type=quote&quote=spot");
    const auto cr = oresmd_parser::parse(uri("oresmd://correlation/ccy-eur-usd?type=quote&quote=pairwise"));
    REQUIRE(oresmd_parser::to_uri(cr, cv).value ==
            "oresmd://correlation/ccy-eur-usd?type=quote&quote=pairwise");
}
