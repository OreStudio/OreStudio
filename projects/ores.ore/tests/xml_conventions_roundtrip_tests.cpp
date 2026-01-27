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

const std::string_view test_suite("ores.ore.conventions.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][conventions]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using ores::ore::domain::conventions;
using namespace ores::logging;

/**
 * @brief Compare two conventions objects by checking collection sizes.
 *
 * For complex nested structures, we validate that all collections have
 * the same size after roundtrip. Detailed field comparison would be
 * extremely verbose given the number of convention types.
 */
void require_conventions_equal(const conventions& original,
                               const conventions& roundtripped) {
    CHECK(roundtripped.Zero.size() == original.Zero.size());
    CHECK(roundtripped.CDS.size() == original.CDS.size());
    CHECK(roundtripped.Deposit.size() == original.Deposit.size());
    CHECK(roundtripped.Future.size() == original.Future.size());
    CHECK(roundtripped.FRA.size() == original.FRA.size());
    CHECK(roundtripped.OIS.size() == original.OIS.size());
    CHECK(roundtripped.Swap.size() == original.Swap.size());
    CHECK(roundtripped.AverageOIS.size() == original.AverageOIS.size());
    CHECK(roundtripped.TenorBasisSwap.size() == original.TenorBasisSwap.size());
    CHECK(roundtripped.TenorBasisTwoSwap.size() == original.TenorBasisTwoSwap.size());
    CHECK(roundtripped.BMABasisSwap.size() == original.BMABasisSwap.size());
    CHECK(roundtripped.FX.size() == original.FX.size());
    CHECK(roundtripped.CrossCurrencyBasis.size() == original.CrossCurrencyBasis.size());
    CHECK(roundtripped.CrossCurrencyFixFloat.size() == original.CrossCurrencyFixFloat.size());
    CHECK(roundtripped.IborIndex.size() == original.IborIndex.size());
    CHECK(roundtripped.OvernightIndex.size() == original.OvernightIndex.size());
    CHECK(roundtripped.SwapIndex.size() == original.SwapIndex.size());
    CHECK(roundtripped.InflationSwap.size() == original.InflationSwap.size());
    CHECK(roundtripped.CmsSpreadOption.size() == original.CmsSpreadOption.size());
    CHECK(roundtripped.CommodityForward.size() == original.CommodityForward.size());
    CHECK(roundtripped.CommodityFuture.size() == original.CommodityFuture.size());
    CHECK(roundtripped.FxOption.size() == original.FxOption.size());
    CHECK(roundtripped.FxOptionTimeWeighting.size() == original.FxOptionTimeWeighting.size());
    CHECK(roundtripped.ZeroInflationIndex.size() == original.ZeroInflationIndex.size());
    CHECK(roundtripped.BondYield.size() == original.BondYield.size());

    // Spot check: verify first Zero convention ID if present
    if (!original.Zero.empty()) {
        REQUIRE(!roundtripped.Zero.empty());
        CHECK(std::string(roundtripped.Zero[0].Id) == std::string(original.Zero[0].Id));
    }

    // Spot check: verify first Deposit convention ID if present
    if (!original.Deposit.empty()) {
        REQUIRE(!roundtripped.Deposit.empty());
        CHECK(std::string(roundtripped.Deposit[0].Id) == std::string(original.Deposit[0].Id));
    }

    // Spot check: verify first Swap convention ID if present
    if (!original.Swap.empty()) {
        REQUIRE(!roundtripped.Swap.empty());
        CHECK(std::string(roundtripped.Swap[0].Id) == std::string(original.Swap[0].Id));
    }
}

/**
 * @brief Perform a structural roundtrip test on conventions.
 */
void test_conventions_roundtrip(const std::string& xml_content,
                                const std::string& source_name) {
    auto lg(make_logger(test_suite));

    // Step 1: Parse original XML
    BOOST_LOG_SEV(lg, debug) << "Parsing original XML from: " << source_name;
    conventions original;
    ores::ore::domain::load_data(xml_content, original);

    const std::size_t total_count =
        original.Zero.size() + original.CDS.size() + original.Deposit.size() +
        original.Future.size() + original.FRA.size() + original.OIS.size() +
        original.Swap.size() + original.AverageOIS.size() +
        original.TenorBasisSwap.size() + original.TenorBasisTwoSwap.size() +
        original.BMABasisSwap.size() + original.FX.size() +
        original.CrossCurrencyBasis.size() + original.CrossCurrencyFixFloat.size() +
        original.IborIndex.size() + original.OvernightIndex.size() +
        original.SwapIndex.size() + original.InflationSwap.size() +
        original.CmsSpreadOption.size() + original.CommodityForward.size() +
        original.CommodityFuture.size() + original.FxOption.size() +
        original.FxOptionTimeWeighting.size() + original.ZeroInflationIndex.size() +
        original.BondYield.size();

    BOOST_LOG_SEV(lg, debug) << "Parsed " << total_count << " total conventions";

    // Step 2: Serialize back to XML
    BOOST_LOG_SEV(lg, debug) << "Serializing to XML";
    const std::string serialized = ores::ore::domain::save_data(original);

    // Step 3: Parse the serialized XML
    BOOST_LOG_SEV(lg, debug) << "Parsing serialized XML";
    conventions roundtripped;
    ores::ore::domain::load_data(serialized, roundtripped);

    // Step 4: Compare domain objects
    BOOST_LOG_SEV(lg, debug) << "Comparing original and roundtripped objects";
    require_conventions_equal(original, roundtripped);

    BOOST_LOG_SEV(lg, info) << "Roundtrip test passed for: " << source_name;
}

void test_conventions_roundtrip_from_file(const std::string& relative_path) {
    const auto f = ore_path(relative_path);
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);
    test_conventions_roundtrip(content, f.string());
}

}

// =============================================================================
// File-Based Roundtrip Tests
// =============================================================================

TEST_CASE("conventions_roundtrip_main_conventions_xml", tags) {
    test_conventions_roundtrip_from_file("examples/Input/conventions.xml");
}

TEST_CASE("conventions_roundtrip_minimal_setup", tags) {
    test_conventions_roundtrip_from_file("examples/MinimalSetup/Input/conventions.xml");
}

TEST_CASE("conventions_roundtrip_ore_api", tags) {
    test_conventions_roundtrip_from_file("examples/ORE-API/Input/conventions.xml");
}
