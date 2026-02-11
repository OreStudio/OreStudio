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

const std::string_view test_suite(
    "ores.ore.nettingsetdefinitions.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][nettingsetdefinitions]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using ores::ore::domain::nettingsetdefinitions;
using namespace ores::logging;

void require_nettingsetdefs_equal(const nettingsetdefinitions& original,
                                   const nettingsetdefinitions& roundtripped) {
    REQUIRE(roundtripped.NettingSet.size() == original.NettingSet.size());
    for (size_t i = 0; i < original.NettingSet.size(); ++i) {
        const auto& orig = original.NettingSet.at(i);
        const auto& rt = roundtripped.NettingSet.at(i);
        CHECK(static_cast<bool>(rt.nettingSetGroup.NettingSetId) ==
              static_cast<bool>(orig.nettingSetGroup.NettingSetId));
        if (orig.nettingSetGroup.NettingSetId)
            CHECK(*rt.nettingSetGroup.NettingSetId ==
                  *orig.nettingSetGroup.NettingSetId);
        CHECK(rt.ActiveCSAFlag == orig.ActiveCSAFlag);
        REQUIRE(static_cast<bool>(rt.CSADetails) ==
                static_cast<bool>(orig.CSADetails));
        if (orig.CSADetails) {
            CHECK(rt.CSADetails->Bilateral == orig.CSADetails->Bilateral);
            CHECK(static_cast<bool>(rt.CSADetails->CSACurrency) ==
                  static_cast<bool>(orig.CSADetails->CSACurrency));
            CHECK(rt.CSADetails->ThresholdPay == orig.CSADetails->ThresholdPay);
            CHECK(rt.CSADetails->ThresholdReceive ==
                  orig.CSADetails->ThresholdReceive);
        }
        CHECK(rt.RiskWeight == orig.RiskWeight);
    }
}

void test_roundtrip_from_file(const std::string& relative_path) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path(relative_path);
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    BOOST_LOG_SEV(lg, debug) << "Parsing original XML from: " << f.string();
    nettingsetdefinitions original;
    ores::ore::domain::load_data(content, original);

    BOOST_LOG_SEV(lg, debug) << "Serializing to XML";
    const std::string serialized = ores::ore::domain::save_data(original);

    BOOST_LOG_SEV(lg, debug) << "Parsing serialized XML";
    nettingsetdefinitions roundtripped;
    ores::ore::domain::load_data(serialized, roundtripped);

    require_nettingsetdefs_equal(original, roundtripped);
    BOOST_LOG_SEV(lg, info) << "Roundtrip test passed for: " << f.string();
}

}

TEST_CASE("nettingsetdefinitions_roundtrip_minimal_setup", tags) {
    test_roundtrip_from_file("examples/MinimalSetup/Input/netting.xml");
}

TEST_CASE("nettingsetdefinitions_roundtrip_ore_api", tags) {
    test_roundtrip_from_file("examples/ORE-API/Input/netting.xml");
}

TEST_CASE("nettingsetdefinitions_roundtrip_exposure_with_collateral", tags) {
    test_roundtrip_from_file(
        "examples/ExposureWithCollateral/Input/netting.xml");
}

TEST_CASE("nettingsetdefinitions_roundtrip_initial_margin_dim", tags) {
    test_roundtrip_from_file(
        "examples/InitialMargin/Input/Dim/netting.xml");
}

TEST_CASE("nettingsetdefinitions_roundtrip_xva_risk", tags) {
    test_roundtrip_from_file("examples/XvaRisk/Input/netting.xml");
}
