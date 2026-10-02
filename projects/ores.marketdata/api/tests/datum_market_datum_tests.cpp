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
#include "ores.marketdata.api/datum/market_datum.hpp"
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

const std::string tags("[datum][market_datum]");

using namespace ores::marketdata::datum;

term t(const char* s) {
    return term::parse(s).value();
}
decimal d(const char* s) {
    return decimal::parse(s).value();
}
code c(const char* s) {
    return code::parse(s).value();
}

/// CDS/CREDIT_SPREAD/ACME/SNRFOR/USD/XR14/5Y, in an order the row does not use.
std::vector<field_value> cds_fields() {
    return {{field::term, t("5Y")},
            {field::ccy, std::string("USD")},
            {field::underlying_name, std::string("ACME")},
            {field::seniority, std::string("SNRFOR")},
            {field::doc_clause, std::string("XR14")},
            {field::running_spread, none}};
}

market_datum cds() {
    return market_datum::make(instrument_type::cds, quote_type::credit_spread, cds_fields())
        .value();
}

std::vector<field_value> without(std::vector<field_value> fields, field f) {
    std::erase_if(fields, [f](const auto& fv) { return fv.name == f; });
    return fields;
}

}

TEST_CASE("a_datum_holds_its_fields_in_the_order_its_key_writes_them", tags) {
    const auto datum = cds();
    std::vector<field> order;
    for (const auto& fv : datum.fields())
        order.push_back(fv.name);
    CHECK(order == std::vector<field>{field::underlying_name,
                                      field::seniority,
                                      field::ccy,
                                      field::doc_clause,
                                      field::term,
                                      field::running_spread});
    CHECK(datum.type() == instrument_type::cds);
    CHECK(datum.quote() == quote_type::credit_spread);
    CHECK_FALSE(datum.is_series());
}

TEST_CASE("a_datum_reads_its_fields_with_their_types", tags) {
    const auto datum = cds();
    const auto* name = datum.get<field::underlying_name>();
    REQUIRE(name);
    CHECK(*name == "ACME");
    const auto* term_value = datum.get<field::term>();
    REQUIRE(term_value);
    CHECK(term_value->text() == "5Y");
    CHECK(datum.get<field::running_spread>() == nullptr);
    CHECK(std::holds_alternative<none_t>(datum.at(field::running_spread)));
    CHECK_THROWS_AS(datum.at(field::strike), std::out_of_range);
}

TEST_CASE("a_datum_refuses_fields_that_do_not_match_its_row", tags) {
    using q = quote_type;
    const auto type = instrument_type::cds;

    SECTION("a missing field") {
        CHECK_FALSE(market_datum::make(type, q::credit_spread, without(cds_fields(), field::ccy)));
    }
    SECTION("a field given twice") {
        auto fields = cds_fields();
        fields.push_back({field::ccy, std::string("EUR")});
        CHECK_FALSE(market_datum::make(type, q::credit_spread, fields));
    }
    SECTION("a field the row does not list") {
        auto fields = cds_fields();
        fields.push_back({field::strike, *strike::parse("ATM")});
        CHECK_FALSE(market_datum::make(type, q::credit_spread, fields));
    }
    SECTION("none where the row allows none is accepted") {
        auto fields = without(cds_fields(), field::doc_clause);
        fields.push_back({field::doc_clause, none});
        CHECK(market_datum::make(type, q::credit_spread, fields));
    }
    SECTION("none where the row does not allow it") {
        auto fields = without(cds_fields(), field::underlying_name);
        fields.push_back({field::underlying_name, none});
        CHECK_FALSE(market_datum::make(type, q::credit_spread, fields));
    }
    SECTION("a value of the wrong type") {
        auto fields = without(cds_fields(), field::term);
        fields.push_back({field::term, std::string("5Y")});
        CHECK_FALSE(market_datum::make(type, q::credit_spread, fields));
    }
}

TEST_CASE("a_code_must_be_one_its_field_takes", tags) {
    const auto make = [](code cap_floor) {
        return market_datum::make(instrument_type::zc_inflation_capfloor,
                                  quote_type::price,
                                  {{field::index, std::string("EUHICPXT")},
                                   {field::term, t("10Y")},
                                   {field::cap_floor, cap_floor},
                                   {field::strike_level, d("0.01")}});
    };
    CHECK(make(c("C")));
    CHECK(make(c("F")));
    CHECK_FALSE(make(c("X")));

    // ORE reads a time unit in any case, and nothing else.
    const auto shape = [](code unit) {
        return market_datum::make(instrument_type::shape_profile,
                                  quote_type::shape_factor,
                                  {{field::quote_name, std::string("PJM_WH_RT")},
                                   {field::delivery_date, t("2027-02-02")},
                                   {field::start_time_in_sec, d("0")},
                                   {field::time_unit, unit},
                                   {field::dst, none}});
    };
    CHECK(shape(c("HOUR")));
    CHECK(shape(c("hour")));
    CHECK_FALSE(shape(c("UNIT")));
}

TEST_CASE("free_text_keeps_its_case", tags) {
    const auto datum = market_datum::make(instrument_type::equity_spot,
                                          quote_type::price,
                                          {{field::eq_name, std::string("Lufthansa")},
                                           {field::ccy, std::string("EUR")}})
                           .value();
    CHECK(*datum.get<field::eq_name>() == "Lufthansa");
}

TEST_CASE("free_text_cannot_be_empty", tags) {
    const auto datum = market_datum::make(
        instrument_type::equity_spot,
        quote_type::price,
        {{field::eq_name, std::string()}, {field::ccy, std::string("EUR")}});
    REQUIRE_FALSE(datum);
    CHECK(datum.error().contains("eq_name"));

    const auto series = market_datum::make_series(
        instrument_type::equity_spot,
        quote_type::price,
        {{field::eq_name, std::string()}, {field::ccy, std::string("EUR")}});
    CHECK_FALSE(series);
}

TEST_CASE("two_datums_are_equal_when_every_part_is", tags) {
    CHECK(cds() == cds());
    auto fields = without(cds_fields(), field::term);
    fields.push_back({field::term, t("10Y")});
    const auto other =
        market_datum::make(instrument_type::cds, quote_type::credit_spread, fields).value();
    CHECK(cds() != other);
    const auto priced =
        market_datum::make(instrument_type::cds, quote_type::price, cds_fields()).value();
    CHECK(cds() != priced);
}

TEST_CASE("series_of_drops_exactly_the_coordinate_fields", tags) {
    const auto datum = cds();
    const auto series = series_of(datum);
    CHECK(series.is_series());
    CHECK(series.type() == datum.type());
    CHECK(series.quote() == datum.quote());
    CHECK_FALSE(series.holds(field::term));
    for (const auto& spec : schema_of(instrument_type::cds).fields) {
        INFO("field: " << name_of(spec.name));
        CHECK(series.holds(spec.name) == (spec.role == field_role::identity));
    }

    // Two points on one CDS curve share a series.
    auto fields = without(cds_fields(), field::term);
    fields.push_back({field::term, t("10Y")});
    const auto other =
        market_datum::make(instrument_type::cds, quote_type::credit_spread, fields).value();
    CHECK(series_of(other) == series);
    CHECK(series_of(series) == series);
}

TEST_CASE("a_series_refuses_a_coordinate", tags) {
    CHECK_FALSE(market_datum::make_series(instrument_type::fx_fwd,
                                          quote_type::rate,
                                          {{field::unit_ccy, std::string("EUR")},
                                           {field::ccy, std::string("USD")},
                                           {field::term, t("1Y")}}));
    CHECK(market_datum::make_series(
        instrument_type::fx_fwd,
        quote_type::rate,
        {{field::unit_ccy, std::string("EUR")}, {field::ccy, std::string("USD")}}));
}

TEST_CASE("every_name_reads_back_to_what_it_names", tags) {
    for (std::size_t i = 0; i < instrument_type_count; ++i) {
        const auto type = static_cast<instrument_type>(i);
        CHECK(instrument_type_named(ore_name(type)) == type);
    }
    for (std::size_t i = 0; i < quote_type_count; ++i) {
        const auto quote = static_cast<quote_type>(i);
        CHECK(quote_type_named(ore_name(quote)) == quote);
    }
    for (std::size_t i = 0; i < field_count; ++i) {
        const auto f = static_cast<field>(i);
        CHECK(field_named(name_of(f)) == f);
    }
    CHECK_FALSE(instrument_type_named("FX"));
    CHECK_FALSE(field_named("settle"));
}

TEST_CASE("every_field_is_used_by_some_row_and_every_code_field_has_a_vocabulary", tags) {
    std::set<field> used;
    for (const auto& row : schema) {
        for (const auto& spec : row.fields)
            used.insert(spec.name);
    }
    for (std::size_t i = 0; i < field_count; ++i) {
        const auto f = static_cast<field>(i);
        INFO("field: " << name_of(f));
        CHECK(used.contains(f));
        CHECK(codes_of(f).empty() == (kind_of(f) != value_kind::code));
    }
}

TEST_CASE("every_asset_class_names_itself_and_is_used_by_some_row", tags) {
    std::set<asset_class> used;
    for (const auto& row : schema)
        used.insert(row.asset);
    for (std::size_t i = 0; i < asset_class_count; ++i) {
        const auto a = static_cast<asset_class>(i);
        INFO("asset class: " << name_of(a));
        CHECK(asset_class_named(name_of(a)) == a);
        CHECK(used.contains(a));
    }
    CHECK_FALSE(asset_class_named("interest_rates"));
}
