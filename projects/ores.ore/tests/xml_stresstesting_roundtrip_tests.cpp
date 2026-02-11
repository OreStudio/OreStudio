/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.ore/domain/domain.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

namespace {

const std::string_view test_suite("ores.ore.stresstesting.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][stresstesting]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using ores::ore::domain::stresstesting;
using namespace ores::logging;

void require_stresstesting_equal(const stresstesting& original,
                                  const stresstesting& roundtripped) {
    CHECK(static_cast<bool>(roundtripped.UseSpreadedTermStructures) ==
          static_cast<bool>(original.UseSpreadedTermStructures));
    REQUIRE(roundtripped.StressTest.size() == original.StressTest.size());
    for (size_t i = 0; i < original.StressTest.size(); ++i) {
        const auto& orig = original.StressTest.at(i);
        const auto& rt = roundtripped.StressTest.at(i);
        CHECK(rt.id == orig.id);
        CHECK(static_cast<bool>(rt.ParShifts) ==
              static_cast<bool>(orig.ParShifts));
        CHECK(static_cast<bool>(rt.DiscountCurves) ==
              static_cast<bool>(orig.DiscountCurves));
        CHECK(static_cast<bool>(rt.FxSpots) ==
              static_cast<bool>(orig.FxSpots));
        CHECK(static_cast<bool>(rt.EquitySpots) ==
              static_cast<bool>(orig.EquitySpots));
        CHECK(static_cast<bool>(rt.SurvivalProbabilities) ==
              static_cast<bool>(orig.SurvivalProbabilities));
    }
}

void test_roundtrip_from_file(const std::string& relative_path) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path(relative_path);
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    BOOST_LOG_SEV(lg, debug) << "Parsing original XML from: " << f.string();
    stresstesting original;
    ores::ore::domain::load_data(content, original);

    BOOST_LOG_SEV(lg, debug) << "Serializing to XML";
    const std::string serialized = ores::ore::domain::save_data(original);

    BOOST_LOG_SEV(lg, debug) << "Parsing serialized XML";
    stresstesting roundtripped;
    ores::ore::domain::load_data(serialized, roundtripped);

    require_stresstesting_equal(original, roundtripped);
    BOOST_LOG_SEV(lg, info) << "Roundtrip test passed for: " << f.string();
}

}

TEST_CASE("stresstesting_roundtrip_market_risk", tags) {
    test_roundtrip_from_file("examples/MarketRisk/Input/stresstest.xml");
}

TEST_CASE("stresstesting_roundtrip_xva_risk", tags) {
    test_roundtrip_from_file("examples/XvaRisk/Input/stresstest.xml");
}
