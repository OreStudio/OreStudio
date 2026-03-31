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
#include "ores.marketdata.api/domain/market_series.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_series_json_io.hpp" // IWYU pragma: keep.

namespace {

using ores::marketdata::domain::market_series;
using ores::marketdata::domain::asset_class;
using ores::marketdata::domain::series_subclass;

const std::string_view test_suite("ores.marketdata.api.tests");
const std::string tags("[domain]");

market_series make_eur_discount_series() {
    market_series s;
    s.version = 1;
    s.series_type = "DISCOUNT";
    s.metric = "RATE";
    s.qualifier = "EUR";
    s.asset_class = asset_class::rates;
    s.subclass = series_subclass::yield;
    s.is_scalar = false;
    s.modified_by = "system";
    s.performed_by = "system";
    s.change_reason_code = "system.new";
    s.change_commentary = "Test data";
    s.recorded_at = std::chrono::system_clock::now();
    return s;
}

}

using namespace ores::logging;

TEST_CASE("create_market_series_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_eur_discount_series();
    BOOST_LOG_SEV(lg, info) << "Market series: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.series_type == "DISCOUNT");
    CHECK(sut.metric == "RATE");
    CHECK(sut.qualifier == "EUR");
    CHECK(sut.asset_class == asset_class::rates);
    CHECK(sut.subclass == series_subclass::yield);
    CHECK(sut.is_scalar == false);
}

TEST_CASE("create_scalar_fx_spot_series", tags) {
    auto lg(make_logger(test_suite));

    market_series sut;
    sut.version = 1;
    sut.series_type = "FX";
    sut.metric = "RATE";
    sut.qualifier = "EUR/USD";
    sut.asset_class = asset_class::fx;
    sut.subclass = series_subclass::spot;
    sut.is_scalar = true;
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "FX spot series: " << sut;

    CHECK(sut.series_type == "FX");
    CHECK(sut.qualifier == "EUR/USD");
    CHECK(sut.asset_class == asset_class::fx);
    CHECK(sut.subclass == series_subclass::spot);
    CHECK(sut.is_scalar == true);
}

TEST_CASE("market_series_json_serialisation", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_eur_discount_series();

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();
    BOOST_LOG_SEV(lg, info) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("DISCOUNT") != std::string::npos);
    CHECK(json_output.find("EUR") != std::string::npos);
}

TEST_CASE("create_swaption_vol_series", tags) {
    auto lg(make_logger(test_suite));

    market_series sut;
    sut.version = 1;
    sut.series_type = "SWAPTION";
    sut.metric = "RATE_LNVOL";
    sut.qualifier = "EUR";
    sut.asset_class = asset_class::rates;
    sut.subclass = series_subclass::volatility;
    sut.is_scalar = false;
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Swaption vol series: " << sut;

    CHECK(sut.series_type == "SWAPTION");
    CHECK(sut.asset_class == asset_class::rates);
    CHECK(sut.subclass == series_subclass::volatility);
    CHECK(sut.is_scalar == false);
}

TEST_CASE("create_market_series_with_faker", tags) {
    auto lg(make_logger(test_suite));

    market_series sut;
    sut.version = faker::number::integer(1, 10);
    sut.series_type = "MM";
    sut.metric = "RATE";
    sut.qualifier = std::string(faker::finance::currencyCode());
    sut.asset_class = asset_class::rates;
    sut.subclass = series_subclass::yield;
    sut.is_scalar = false;
    sut.modified_by = std::string(faker::internet::username());
    sut.performed_by = std::string(faker::internet::username());
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Synthetic test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Faker market series: " << sut;

    CHECK(sut.version >= 1);
    CHECK(!sut.qualifier.empty());
    CHECK(!sut.modified_by.empty());
}
