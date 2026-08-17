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
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/market_series_json_io.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <boost/uuid/uuid_generators.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cctype>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <sstream>

namespace {

using ores::marketdata::domain::market_series;

const std::string_view test_suite("ores.marketdata.api.tests");
const std::string tags("[domain]");

market_series make_eur_discount_series() {
    market_series s;
    s.version = 1;
    s.oresmd_uri = "oresmd://ir/eur?role=discount&type=curve";
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
    CHECK(sut.oresmd_uri == "oresmd://ir/eur?role=discount&type=curve");
    CHECK(sut.derivation_kind == "OBSERVED");
}

TEST_CASE("create_scalar_fx_spot_series", tags) {
    auto lg(make_logger(test_suite));

    market_series sut;
    sut.version = 1;
    sut.oresmd_uri = "oresmd://fx/eurusd?type=quote&quote=spot";
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "FX spot series: " << sut;

    CHECK(sut.oresmd_uri == "oresmd://fx/eurusd?type=quote&quote=spot");
}

TEST_CASE("market_series_json_serialisation", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_eur_discount_series();

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();
    BOOST_LOG_SEV(lg, info) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("oresmd://ir/eur?role=discount&type=curve") != std::string::npos);
}

TEST_CASE("create_swaption_vol_series", tags) {
    auto lg(make_logger(test_suite));

    market_series sut;
    sut.version = 1;
    sut.oresmd_uri = "oresmd://ir/eur?type=vol";
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Swaption vol series: " << sut;

    CHECK(sut.oresmd_uri == "oresmd://ir/eur?type=vol");
}

TEST_CASE("create_market_series_with_faker", tags) {
    auto lg(make_logger(test_suite));

    market_series sut;
    sut.version = faker::number::integer(1, 10);
    const auto currency = std::string(faker::finance::currencyCode());
    std::string ccy;
    ccy.reserve(currency.size());
    std::transform(currency.begin(), currency.end(), std::back_inserter(ccy),
                   [](unsigned char c) { return std::tolower(c); });
    sut.oresmd_uri =
        "oresmd://ir/" + ccy + "?index=libor&tenor=3m&type=quote&metric=rate&quote=mm&point=1m";
    sut.modified_by = std::string(faker::internet::username());
    sut.performed_by = std::string(faker::internet::username());
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Synthetic test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Faker market series: " << sut;

    CHECK(sut.version >= 1);
    CHECK(sut.oresmd_uri.find(ccy) != std::string::npos);
    CHECK(!sut.modified_by.empty());
}
