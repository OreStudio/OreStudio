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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/repository/series_identity_shape_check.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <stdexcept>
#include <string>
#include <vector>

/**
 * @file series_identity_shape_check_tests.cpp
 * @brief States a family's identity shape and asks the guard what it makes of a row.
 *
 * The guard is pure, so every case here is a shape, a row and an outcome: no
 * database, no service, no clock.
 */

namespace {

using ores::marketdata::domain::market_series;
using ores::marketdata::domain::market_series_identity;
using ores::marketdata::domain::series_identity_shape;
using ores::marketdata::repository::check_identity_kind;
using ores::marketdata::repository::check_identity_shape;
using ores::marketdata::repository::identity_shape_book;
using ores::marketdata::repository::states_identity_shape;

constexpr const char* shape_tags = "[series_identity][shape]";

series_identity_shape stated(const std::string& subclass,
                             const std::string& index_family,
                             const std::string& field,
                             const std::string& requirement) {
    series_identity_shape shape;
    shape.asset_class = "ir";
    shape.series_subclass = subclass;
    shape.index_family = index_family;
    shape.field = field;
    shape.requirement = requirement;
    return shape;
}

market_series series_of(const std::string& subclass, const std::string& uri) {
    market_series series;
    series.oresmd_uri = uri;
    series.series_subclass = subclass;
    return series;
}

market_series_identity ibor_fixing(const std::string& ccy,
                                   const std::string& index_name,
                                   const std::string& unit_ccy = "") {
    market_series_identity row;
    row.identity_kind = "index";
    row.asset_class = "ir";
    row.index = "ibor";
    row.ccy = ccy;
    row.index_name = index_name;
    row.unit_ccy = unit_ccy;
    return row;
}

}

TEST_CASE("an_ibor_fixing_states_its_currency_its_index_and_its_name", shape_tags) {
    auto lg(make_logger(test_suite));

    const std::vector<series_identity_shape> shape{
        stated("index_fixing", "ibor", "ccy", "required"),
        stated("index_fixing", "ibor", "index", "required"),
        stated("index_fixing", "ibor", "index_name", "required"),
        stated("index_fixing", "ibor", "unit_ccy", "forbidden")};
    const auto series =
        series_of("index_fixing", "oresmd://ir/USD?type=fixing&index=ibor&name=SOFR");
    const auto row = ibor_fixing("USD", "SOFR");

    CHECK_NOTHROW(check_identity_shape(shape, series, row));
    CHECK(states_identity_shape(shape, series, row));
    CHECK(row.index_name == "SOFR");
    CHECK(row.unit_ccy.empty());
}

TEST_CASE("an_ibor_fixing_that_misses_a_required_field_is_refused_by_name", shape_tags) {
    auto lg(make_logger(test_suite));

    const std::vector<series_identity_shape> shape{
        stated("index_fixing", "ibor", "index_name", "required")};
    const auto series = series_of("index_fixing", "oresmd://ir/USD?type=fixing&index=ibor");

    CHECK_THROWS_WITH(check_identity_shape(shape, series, ibor_fixing("USD", "")),
                      Catch::Matchers::ContainsSubstring("field 'index_name' is required"));
}

TEST_CASE("an_ibor_fixing_that_carries_a_unit_currency_is_refused_by_name", shape_tags) {
    auto lg(make_logger(test_suite));

    const std::vector<series_identity_shape> shape{
        stated("index_fixing", "ibor", "unit_ccy", "forbidden")};
    const auto series =
        series_of("index_fixing", "oresmd://ir/USD?type=fixing&index=ibor&name=SOFR&unit_ccy=EUR");

    CHECK_THROWS_WITH(check_identity_shape(shape, series, ibor_fixing("USD", "SOFR", "EUR")),
                      Catch::Matchers::ContainsSubstring("field 'unit_ccy' is forbidden"));
}

TEST_CASE("a_family_no_shape_states_is_projected_unguarded_and_reported", shape_tags) {
    auto lg(make_logger(test_suite));

    // A stated family is another family's shape: the guard must not borrow it.
    const std::vector<series_identity_shape> shape{
        stated("index_fixing", "ibor", "ccy", "required")};
    const auto series = series_of("yield", "oresmd://ir/USD?type=series&instrument=swap");
    auto row = ibor_fixing("", "");
    row.asset_class = "ir";
    row.index.clear();

    CHECK_NOTHROW(check_identity_shape(shape, series, row));
    CHECK_FALSE(states_identity_shape(shape, series, row));
}

TEST_CASE("a_forbidden_field_is_refused_while_a_field_nobody_states_is_left_alone", shape_tags) {
    auto lg(make_logger(test_suite));

    const std::vector<series_identity_shape> shape{
        stated("index_fixing", "ibor", "unit_ccy", "forbidden")};
    const auto series =
        series_of("index_fixing", "oresmd://ir/GBP?type=fixing&index=ibor&name=SONIA");
    auto row = ibor_fixing("GBP", "SONIA");
    // tenor is not mentioned by the family, so the guard has no opinion on it.
    row.tenor = "3M";

    CHECK_NOTHROW(check_identity_shape(shape, series, row));
}

TEST_CASE("a_shape_that_names_a_field_an_identity_does_not_hold_is_refused", shape_tags) {
    auto lg(make_logger(test_suite));

    // A misspelt forbidden field would otherwise pass every row of its family.
    const std::vector<series_identity_shape> shape{
        stated("index_fixing", "ibor", "unit_currency", "forbidden")};
    const auto series =
        series_of("index_fixing", "oresmd://ir/USD?type=fixing&index=ibor&name=SOFR");

    CHECK_THROWS_WITH(check_identity_shape(shape, series, ibor_fixing("USD", "SOFR")),
                      Catch::Matchers::ContainsSubstring(
                          "shape names field 'unit_currency', which an identity does not hold"));

    // The backfill counts an std::invalid_argument as a refused series. A fault of the
    // catalogue must not be counted that way, so it is a logic_error and nothing more.
    bool refused_as_a_series = false;
    bool faulted_the_catalogue = false;
    try {
        check_identity_shape(shape, series, ibor_fixing("USD", "SOFR"));
    } catch (const std::invalid_argument&) {
        refused_as_a_series = true;
    } catch (const std::logic_error&) {
        faulted_the_catalogue = true;
    }
    CHECK_FALSE(refused_as_a_series);
    CHECK(faulted_the_catalogue);
}

TEST_CASE("a_book_answers_as_the_stated_rows_do", shape_tags) {
    auto lg(make_logger(test_suite));

    const std::vector<series_identity_shape> shape{
        stated("index_fixing", "ibor", "index_name", "required"),
        stated("index_fixing", "ibor", "unit_ccy", "forbidden")};
    const identity_shape_book book(shape);
    const auto series =
        series_of("index_fixing", "oresmd://ir/USD?type=fixing&index=ibor&name=SOFR");

    CHECK_NOTHROW(book.check(series, ibor_fixing("USD", "SOFR")));
    CHECK(book.states(series, ibor_fixing("USD", "SOFR")));
    CHECK_THROWS_AS(book.check(series, ibor_fixing("USD", "")), std::invalid_argument);
    CHECK_THROWS_AS(book.check(series, ibor_fixing("USD", "SOFR", "EUR")), std::invalid_argument);
    CHECK_FALSE(book.states(series_of("yield", series.oresmd_uri), ibor_fixing("USD", "SOFR")));
}

TEST_CASE("a_fixing_whose_series_subclass_says_something_else_is_refused_by_name", shape_tags) {
    auto lg(make_logger(test_suite));

    // The shape is keyed on the subclass, so a fixing that states another one is
    // held to no shape at all: the kind has to be checked before the family is read.
    const auto series = series_of("yield", "oresmd://ir/USD?type=fixing&index=ibor&name=SOFR");

    CHECK_NOTHROW(check_identity_kind(series_of("index_fixing", series.oresmd_uri),
                                      ibor_fixing("USD", "SOFR")));
    CHECK_THROWS_WITH(
        check_identity_kind(series, ibor_fixing("USD", "SOFR")),
        Catch::Matchers::ContainsSubstring("so its series subclass is 'index_fixing' and not"));
}

TEST_CASE("a_series_that_is_not_an_index_is_left_to_its_own_writers", shape_tags) {
    auto lg(make_logger(test_suite));

    // A datum series carries no index, so the kind says nothing about its subclass.
    auto row = ibor_fixing("USD", "SOFR");
    row.identity_kind = "series";

    CHECK_NOTHROW(check_identity_kind(
        series_of("yield", "oresmd://ir/USD?type=series&instrument=swap"), row));
}
