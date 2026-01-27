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

namespace {

const std::string_view test_suite("ores.ore.simulation.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][simulation]");

using ores::ore::domain::simulation;
using namespace ores::logging;

/**
 * @brief Compare two simulation objects by checking optional section presence.
 */
void require_simulation_equal(const simulation& original,
                               const simulation& roundtripped) {
    CHECK(static_cast<bool>(roundtripped.Parameters) ==
          static_cast<bool>(original.Parameters));
    CHECK(static_cast<bool>(roundtripped.CrossAssetModel) ==
          static_cast<bool>(original.CrossAssetModel));
    CHECK(static_cast<bool>(roundtripped.Market) ==
          static_cast<bool>(original.Market));

    // Check Parameters details if present
    if (original.Parameters && roundtripped.Parameters) {
        // Grid and Calendar are required fields, check their content
        CHECK(std::string(roundtripped.Parameters->Grid) ==
              std::string(original.Parameters->Grid));
        CHECK(std::string(roundtripped.Parameters->Calendar) ==
              std::string(original.Parameters->Calendar));
        CHECK(roundtripped.Parameters->Seed == original.Parameters->Seed);
        CHECK(roundtripped.Parameters->Samples == original.Parameters->Samples);
    }
}

/**
 * @brief Perform a structural roundtrip test on simulation.
 */
void test_simulation_roundtrip(const std::string& xml_content,
                                const std::string& source_name) {
    auto lg(make_logger(test_suite));

    // Step 1: Parse original XML
    BOOST_LOG_SEV(lg, debug) << "Parsing original XML from: " << source_name;
    simulation original;
    ores::ore::domain::load_data(xml_content, original);

    BOOST_LOG_SEV(lg, debug) << "Parsed simulation with Parameters: "
                             << static_cast<bool>(original.Parameters)
                             << ", CrossAssetModel: "
                             << static_cast<bool>(original.CrossAssetModel)
                             << ", Market: "
                             << static_cast<bool>(original.Market);

    // Step 2: Serialize back to XML
    BOOST_LOG_SEV(lg, debug) << "Serializing to XML";
    const std::string serialized = ores::ore::domain::save_data(original);

    // Step 3: Parse the serialized XML
    BOOST_LOG_SEV(lg, debug) << "Parsing serialized XML";
    simulation roundtripped;
    ores::ore::domain::load_data(serialized, roundtripped);

    // Step 4: Compare domain objects
    BOOST_LOG_SEV(lg, debug) << "Comparing original and roundtripped objects";
    require_simulation_equal(original, roundtripped);

    BOOST_LOG_SEV(lg, info) << "Roundtrip test passed for: " << source_name;
}

}

// =============================================================================
// File-Based Roundtrip Tests
// =============================================================================

// Note: Most ORE simulation XML files contain multiple LGM elements
// where the XSD schema only allows 1. Tests are limited to compatible files.

TEST_CASE("simulation_roundtrip_simple", tags) {
    // Test with a minimal inline simulation
    const std::string xml = R"(
<Simulation>
  <Parameters>
    <Grid>1M,3M,6M,1Y</Grid>
    <Calendar>EUR</Calendar>
    <Sequence>SobolBrownianBridge</Sequence>
    <Seed>42</Seed>
    <Samples>1000</Samples>
  </Parameters>
</Simulation>
)";

    test_simulation_roundtrip(xml, "simple_inline");
}

