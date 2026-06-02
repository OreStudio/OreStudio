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
#include "ores.marketdata.api/domain/market_fixing.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_fixing_json_io.hpp" // IWYU pragma: keep.

namespace {

using ores::marketdata::domain::market_fixing;

const std::string_view test_suite("ores.marketdata.api.tests");
const std::string tags("[domain]");

market_fixing make_euribor_fixing(const std::string& value = "0.038940") {
    static boost::uuids::random_generator gen;
    market_fixing f;
    f.id = gen();
    f.series_id = gen();
    f.fixing_date = std::chrono::year{2024} / std::chrono::month{3} /
                    std::chrono::day{20};
    f.value = value;
    f.source = "ECB";
    f.recorded_at = std::chrono::system_clock::now();
    return f;
}

}

using namespace ores::logging;

TEST_CASE("create_fixing_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_euribor_fixing("0.038940");
    BOOST_LOG_SEV(lg, info) << "Fixing: " << sut;

    CHECK(!sut.id.is_nil());
    CHECK(!sut.series_id.is_nil());
    CHECK(sut.fixing_date == (std::chrono::year{2024} / std::chrono::month{3} /
                              std::chrono::day{20}));
    CHECK(sut.value == "0.038940");
    CHECK(sut.source.has_value());
    CHECK(sut.source.value() == "ECB");
}

TEST_CASE("create_fixing_without_source", tags) {
    auto lg(make_logger(test_suite));

    static boost::uuids::random_generator gen;
    market_fixing sut;
    sut.id = gen();
    sut.series_id = gen();
    sut.fixing_date = std::chrono::year{2024} / std::chrono::month{1} /
                      std::chrono::day{15};
    sut.value = "0.052300";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Fixing without source: " << sut;

    CHECK(!sut.source.has_value());
    CHECK(sut.value == "0.052300");
}

TEST_CASE("market_fixing_json_serialisation", tags) {
    auto lg(make_logger(test_suite));

    auto sut = make_euribor_fixing("0.038940");

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();
    BOOST_LOG_SEV(lg, info) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("0.038940") != std::string::npos);
    CHECK(json_output.find("2024") != std::string::npos);
    CHECK(json_output.find("ECB") != std::string::npos);
}

TEST_CASE("create_fixing_history_series", tags) {
    auto lg(make_logger(test_suite));

    std::vector<market_fixing> fixings;
    fixings.reserve(5);
    for (int day = 1; day <= 5; ++day) {
        static boost::uuids::random_generator gen;
        market_fixing f;
        f.id = gen();
        f.series_id = gen();
        f.fixing_date = std::chrono::year{2024} / std::chrono::month{3} /
                        std::chrono::day{static_cast<unsigned>(day)};
        f.value = std::to_string(0.039 + day * 0.0001);
        f.recorded_at = std::chrono::system_clock::now();
        fixings.push_back(f);
    }
    BOOST_LOG_SEV(lg, info) << "Fixing history size: " << fixings.size();

    CHECK(fixings.size() == 5);
    for (const auto& f : fixings) {
        CHECK(!f.value.empty());
        CHECK(!f.id.is_nil());
    }
}
