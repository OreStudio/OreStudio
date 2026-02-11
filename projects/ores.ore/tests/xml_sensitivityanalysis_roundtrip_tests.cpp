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

const std::string_view test_suite("ores.ore.sensitivityanalysis.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][sensitivityanalysis]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using ores::ore::domain::sensitivityanalysis;
using namespace ores::logging;

void require_sensitivityanalysis_equal(const sensitivityanalysis& original,
                                        const sensitivityanalysis& roundtripped) {
    CHECK(roundtripped.DiscountCurves.DiscountCurve.size() ==
          original.DiscountCurves.DiscountCurve.size());
    CHECK(static_cast<bool>(roundtripped.IndexCurves) ==
          static_cast<bool>(original.IndexCurves));
    if (original.IndexCurves)
        CHECK(roundtripped.IndexCurves->IndexCurve.size() ==
              original.IndexCurves->IndexCurve.size());
    CHECK(static_cast<bool>(roundtripped.FxSpots) ==
          static_cast<bool>(original.FxSpots));
    CHECK(static_cast<bool>(roundtripped.SwaptionVolatilities) ==
          static_cast<bool>(original.SwaptionVolatilities));
    CHECK(static_cast<bool>(roundtripped.CapFloorVolatilities) ==
          static_cast<bool>(original.CapFloorVolatilities));
    CHECK(static_cast<bool>(roundtripped.CreditCurves) ==
          static_cast<bool>(original.CreditCurves));
    CHECK(roundtripped.ParConversion == original.ParConversion);
    CHECK(static_cast<bool>(roundtripped.ComputeGamma) ==
          static_cast<bool>(original.ComputeGamma));
    CHECK(static_cast<bool>(roundtripped.CrossGammaFilter) ==
          static_cast<bool>(original.CrossGammaFilter));
}

void test_roundtrip_from_file(const std::string& relative_path) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path(relative_path);
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    BOOST_LOG_SEV(lg, debug) << "Parsing original XML from: " << f.string();
    sensitivityanalysis original;
    ores::ore::domain::load_data(content, original);

    BOOST_LOG_SEV(lg, debug) << "Serializing to XML";
    const std::string serialized = ores::ore::domain::save_data(original);

    BOOST_LOG_SEV(lg, debug) << "Parsing serialized XML";
    sensitivityanalysis roundtripped;
    ores::ore::domain::load_data(serialized, roundtripped);

    require_sensitivityanalysis_equal(original, roundtripped);
    BOOST_LOG_SEV(lg, info) << "Roundtrip test passed for: " << f.string();
}

}

TEST_CASE("sensitivityanalysis_roundtrip_exposure", tags) {
    test_roundtrip_from_file("examples/Exposure/Input/sensitivity.xml");
}

TEST_CASE("sensitivityanalysis_roundtrip_market_risk", tags) {
    test_roundtrip_from_file("examples/MarketRisk/Input/sensitivity.xml");
}
