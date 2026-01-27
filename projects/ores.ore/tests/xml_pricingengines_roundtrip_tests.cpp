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

const std::string_view test_suite("ores.ore.pricingengines.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][pricingengines]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using ores::ore::domain::pricingengines;
using namespace ores::logging;

/**
 * @brief Compare two pricingengines objects by checking collection sizes.
 */
void require_pricingengines_equal(const pricingengines& original,
                                   const pricingengines& roundtripped) {
    CHECK(roundtripped.Product.size() == original.Product.size());
    CHECK(static_cast<bool>(roundtripped.GlobalParameters) ==
          static_cast<bool>(original.GlobalParameters));

    // Spot check: verify first Product type if present
    if (!original.Product.empty()) {
        REQUIRE(!roundtripped.Product.empty());
        CHECK(std::string(roundtripped.Product[0].type) ==
              std::string(original.Product[0].type));
    }
}

/**
 * @brief Perform a structural roundtrip test on pricingengines.
 */
void test_pricingengines_roundtrip(const std::string& xml_content,
                                    const std::string& source_name) {
    auto lg(make_logger(test_suite));

    // Step 1: Parse original XML
    BOOST_LOG_SEV(lg, debug) << "Parsing original XML from: " << source_name;
    pricingengines original;
    ores::ore::domain::load_data(xml_content, original);

    BOOST_LOG_SEV(lg, debug) << "Parsed " << original.Product.size() << " products";

    // Step 2: Serialize back to XML
    BOOST_LOG_SEV(lg, debug) << "Serializing to XML";
    const std::string serialized = ores::ore::domain::save_data(original);

    // Step 3: Parse the serialized XML
    BOOST_LOG_SEV(lg, debug) << "Parsing serialized XML";
    pricingengines roundtripped;
    ores::ore::domain::load_data(serialized, roundtripped);

    // Step 4: Compare domain objects
    BOOST_LOG_SEV(lg, debug) << "Comparing original and roundtripped objects";
    require_pricingengines_equal(original, roundtripped);

    BOOST_LOG_SEV(lg, info) << "Roundtrip test passed for: " << source_name;
}

}

// =============================================================================
// File-Based Roundtrip Tests
// =============================================================================

TEST_CASE("pricingengines_roundtrip_main_xml", tags) {
    const auto f = ore_path("examples/Input/pricingengine.xml");
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    test_pricingengines_roundtrip(content, f.string());
}

TEST_CASE("pricingengines_roundtrip_ore_api", tags) {
    const auto f = ore_path("examples/ORE-API/Input/pricingengine.xml");
    using ores::platform::filesystem::file;

    if (!std::filesystem::exists(f)) {
        SKIP("File not found: " << f.string());
    }

    const std::string content = file::read_content(f);
    test_pricingengines_roundtrip(content, f.string());
}

TEST_CASE("pricingengines_roundtrip_minimal_setup", tags) {
    const auto f = ore_path("examples/MinimalSetup/Input/pricingengine.xml");
    using ores::platform::filesystem::file;

    if (!std::filesystem::exists(f)) {
        SKIP("File not found: " << f.string());
    }

    const std::string content = file::read_content(f);
    test_pricingengines_roundtrip(content, f.string());
}

