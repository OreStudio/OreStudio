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

#include <filesystem>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

namespace {

const std::string_view test_suite("ores.ore.todaysmarket.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][todaysmarket]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using ores::ore::domain::todaysmarket;
using namespace ores::logging;

/**
 * @brief Compare two todaysmarket objects by checking collection sizes.
 */
void require_todaysmarket_equal(const todaysmarket& original,
                                 const todaysmarket& roundtripped) {
    CHECK(roundtripped.Configuration.size() == original.Configuration.size());
    CHECK(roundtripped.YieldCurves.size() == original.YieldCurves.size());
    CHECK(roundtripped.DiscountingCurves.size() == original.DiscountingCurves.size());
    CHECK(roundtripped.IndexForwardingCurves.size() == original.IndexForwardingCurves.size());
    CHECK(roundtripped.SwapIndexCurves.size() == original.SwapIndexCurves.size());
    CHECK(roundtripped.ZeroInflationIndexCurves.size() == original.ZeroInflationIndexCurves.size());
    CHECK(roundtripped.YYInflationIndexCurves.size() == original.YYInflationIndexCurves.size());
    CHECK(roundtripped.FxSpots.size() == original.FxSpots.size());
    CHECK(roundtripped.FxVolatilities.size() == original.FxVolatilities.size());
    CHECK(roundtripped.SwaptionVolatilities.size() == original.SwaptionVolatilities.size());
    CHECK(roundtripped.YieldVolatilities.size() == original.YieldVolatilities.size());
    CHECK(roundtripped.CapFloorVolatilities.size() == original.CapFloorVolatilities.size());
    CHECK(roundtripped.CDSVolatilities.size() == original.CDSVolatilities.size());
    CHECK(roundtripped.DefaultCurves.size() == original.DefaultCurves.size());
    CHECK(roundtripped.YYInflationCapFloorVolatilities.size() == original.YYInflationCapFloorVolatilities.size());
    CHECK(roundtripped.ZeroInflationCapFloorVolatilities.size() == original.ZeroInflationCapFloorVolatilities.size());
    CHECK(roundtripped.EquityCurves.size() == original.EquityCurves.size());
    CHECK(roundtripped.EquityVolatilities.size() == original.EquityVolatilities.size());
    CHECK(roundtripped.Securities.size() == original.Securities.size());
    CHECK(roundtripped.BaseCorrelations.size() == original.BaseCorrelations.size());
    CHECK(roundtripped.CommodityCurves.size() == original.CommodityCurves.size());
    CHECK(roundtripped.CommodityVolatilities.size() == original.CommodityVolatilities.size());
    CHECK(roundtripped.Correlations.size() == original.Correlations.size());

    // Spot check: verify first Configuration id if present
    if (!original.Configuration.empty()) {
        REQUIRE(!roundtripped.Configuration.empty());
        CHECK(std::string(roundtripped.Configuration[0].id) ==
              std::string(original.Configuration[0].id));
    }
}

/**
 * @brief Perform a structural roundtrip test on todaysmarket.
 */
void test_todaysmarket_roundtrip(const std::string& xml_content,
                                  const std::string& source_name) {
    auto lg(make_logger(test_suite));

    // Step 1: Parse original XML
    BOOST_LOG_SEV(lg, debug) << "Parsing original XML from: " << source_name;
    todaysmarket original;
    ores::ore::domain::load_data(xml_content, original);

    const std::size_t total_count =
        original.Configuration.size() + original.YieldCurves.size() +
        original.DiscountingCurves.size() + original.IndexForwardingCurves.size() +
        original.SwapIndexCurves.size() + original.FxSpots.size() +
        original.FxVolatilities.size() + original.SwaptionVolatilities.size() +
        original.DefaultCurves.size() + original.EquityCurves.size();

    BOOST_LOG_SEV(lg, debug) << "Parsed " << total_count << " total todaysmarket elements";

    // Step 2: Serialize back to XML
    BOOST_LOG_SEV(lg, debug) << "Serializing to XML";
    const std::string serialized = ores::ore::domain::save_data(original);

    // Step 3: Parse the serialized XML
    BOOST_LOG_SEV(lg, debug) << "Parsing serialized XML";
    todaysmarket roundtripped;
    ores::ore::domain::load_data(serialized, roundtripped);

    // Step 4: Compare domain objects
    BOOST_LOG_SEV(lg, debug) << "Comparing original and roundtripped objects";
    require_todaysmarket_equal(original, roundtripped);

    BOOST_LOG_SEV(lg, info) << "Roundtrip test passed for: " << source_name;
}

void test_todaysmarket_roundtrip_from_file(const std::string& relative_path) {
    const auto f = ore_path(relative_path);
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);
    test_todaysmarket_roundtrip(content, f.string());
}

}

// =============================================================================
// File-Based Roundtrip Tests
// =============================================================================

TEST_CASE("todaysmarket_roundtrip_main_xml", tags) {
    test_todaysmarket_roundtrip_from_file("examples/Input/todaysmarket.xml");
}

TEST_CASE("todaysmarket_roundtrip_ore_api", tags) {
    test_todaysmarket_roundtrip_from_file("examples/ORE-API/Input/todaysmarket.xml");
}

TEST_CASE("todaysmarket_roundtrip_minimal_setup", tags) {
    test_todaysmarket_roundtrip_from_file("examples/MinimalSetup/Input/todaysmarket.xml");
}

