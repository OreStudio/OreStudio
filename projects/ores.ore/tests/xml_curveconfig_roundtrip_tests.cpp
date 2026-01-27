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

const std::string_view test_suite("ores.ore.curveconfig.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][curveconfig]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using ores::ore::domain::curveconfiguration;
using namespace ores::logging;

/**
 * @brief Count total elements in a curveconfiguration.
 */
std::size_t count_elements(const curveconfiguration& cc) {
    std::size_t count = 0;

    if (cc.FXSpots) count += cc.FXSpots->FXSpot.size();
    if (cc.FXVolatilities) count += cc.FXVolatilities->FXVolatility.size();
    if (cc.SwaptionVolatilities) count += cc.SwaptionVolatilities->SwaptionVolatility.size();
    if (cc.YieldVolatilities) count += cc.YieldVolatilities->YieldVolatility.size();
    if (cc.CapFloorVolatilities) count += cc.CapFloorVolatilities->CapFloorVolatility.size();
    if (cc.CDSVolatilities) count += cc.CDSVolatilities->CDSVolatility.size();
    if (cc.DefaultCurves) count += cc.DefaultCurves->DefaultCurve.size();
    if (cc.YieldCurves) count += cc.YieldCurves->YieldCurve.size();
    if (cc.InflationCurves) count += cc.InflationCurves->InflationCurve.size();
    if (cc.InflationCapFloorVolatilities) count += cc.InflationCapFloorVolatilities->InflationCapFloorVolatility.size();
    if (cc.EquityCurves) count += cc.EquityCurves->EquityCurve.size();
    if (cc.EquityVolatilities) count += cc.EquityVolatilities->EquityVolatility.size();
    if (cc.Securities) count += cc.Securities->Security.size();
    if (cc.BaseCorrelations) count += cc.BaseCorrelations->BaseCorrelation.size();
    if (cc.CommodityCurves) count += cc.CommodityCurves->CommodityCurve.size();
    if (cc.CommodityVolatilities) count += cc.CommodityVolatilities->CommodityVolatility.size();
    if (cc.Correlations) count += cc.Correlations->Correlation.size();

    return count;
}

/**
 * @brief Compare two curveconfiguration objects by checking collection sizes.
 */
void require_curveconfig_equal(const curveconfiguration& original,
                                const curveconfiguration& roundtripped) {
    // Check presence of optional sections
    CHECK(static_cast<bool>(roundtripped.ReportConfiguration) ==
          static_cast<bool>(original.ReportConfiguration));
    CHECK(static_cast<bool>(roundtripped.FXSpots) ==
          static_cast<bool>(original.FXSpots));
    CHECK(static_cast<bool>(roundtripped.FXVolatilities) ==
          static_cast<bool>(original.FXVolatilities));
    CHECK(static_cast<bool>(roundtripped.SwaptionVolatilities) ==
          static_cast<bool>(original.SwaptionVolatilities));
    CHECK(static_cast<bool>(roundtripped.YieldVolatilities) ==
          static_cast<bool>(original.YieldVolatilities));
    CHECK(static_cast<bool>(roundtripped.CapFloorVolatilities) ==
          static_cast<bool>(original.CapFloorVolatilities));
    CHECK(static_cast<bool>(roundtripped.CDSVolatilities) ==
          static_cast<bool>(original.CDSVolatilities));
    CHECK(static_cast<bool>(roundtripped.DefaultCurves) ==
          static_cast<bool>(original.DefaultCurves));
    CHECK(static_cast<bool>(roundtripped.YieldCurves) ==
          static_cast<bool>(original.YieldCurves));
    CHECK(static_cast<bool>(roundtripped.InflationCurves) ==
          static_cast<bool>(original.InflationCurves));
    CHECK(static_cast<bool>(roundtripped.InflationCapFloorVolatilities) ==
          static_cast<bool>(original.InflationCapFloorVolatilities));
    CHECK(static_cast<bool>(roundtripped.EquityCurves) ==
          static_cast<bool>(original.EquityCurves));
    CHECK(static_cast<bool>(roundtripped.EquityVolatilities) ==
          static_cast<bool>(original.EquityVolatilities));
    CHECK(static_cast<bool>(roundtripped.Securities) ==
          static_cast<bool>(original.Securities));
    CHECK(static_cast<bool>(roundtripped.BaseCorrelations) ==
          static_cast<bool>(original.BaseCorrelations));
    CHECK(static_cast<bool>(roundtripped.CommodityCurves) ==
          static_cast<bool>(original.CommodityCurves));
    CHECK(static_cast<bool>(roundtripped.CommodityVolatilities) ==
          static_cast<bool>(original.CommodityVolatilities));
    CHECK(static_cast<bool>(roundtripped.Correlations) ==
          static_cast<bool>(original.Correlations));

    // Check collection sizes
    if (original.FXSpots) {
        REQUIRE(static_cast<bool>(roundtripped.FXSpots));
        CHECK(roundtripped.FXSpots->FXSpot.size() == original.FXSpots->FXSpot.size());
    }
    if (original.FXVolatilities) {
        REQUIRE(static_cast<bool>(roundtripped.FXVolatilities));
        CHECK(roundtripped.FXVolatilities->FXVolatility.size() ==
              original.FXVolatilities->FXVolatility.size());
    }
    if (original.SwaptionVolatilities) {
        REQUIRE(static_cast<bool>(roundtripped.SwaptionVolatilities));
        CHECK(roundtripped.SwaptionVolatilities->SwaptionVolatility.size() ==
              original.SwaptionVolatilities->SwaptionVolatility.size());
    }
    if (original.YieldCurves) {
        REQUIRE(static_cast<bool>(roundtripped.YieldCurves));
        CHECK(roundtripped.YieldCurves->YieldCurve.size() ==
              original.YieldCurves->YieldCurve.size());
    }
    if (original.DefaultCurves) {
        REQUIRE(static_cast<bool>(roundtripped.DefaultCurves));
        CHECK(roundtripped.DefaultCurves->DefaultCurve.size() ==
              original.DefaultCurves->DefaultCurve.size());
    }
    if (original.EquityCurves) {
        REQUIRE(static_cast<bool>(roundtripped.EquityCurves));
        CHECK(roundtripped.EquityCurves->EquityCurve.size() ==
              original.EquityCurves->EquityCurve.size());
    }
    if (original.EquityVolatilities) {
        REQUIRE(static_cast<bool>(roundtripped.EquityVolatilities));
        CHECK(roundtripped.EquityVolatilities->EquityVolatility.size() ==
              original.EquityVolatilities->EquityVolatility.size());
    }
    if (original.CommodityCurves) {
        REQUIRE(static_cast<bool>(roundtripped.CommodityCurves));
        CHECK(roundtripped.CommodityCurves->CommodityCurve.size() ==
              original.CommodityCurves->CommodityCurve.size());
    }
    if (original.CommodityVolatilities) {
        REQUIRE(static_cast<bool>(roundtripped.CommodityVolatilities));
        CHECK(roundtripped.CommodityVolatilities->CommodityVolatility.size() ==
              original.CommodityVolatilities->CommodityVolatility.size());
    }

    // Spot check: verify first FX volatility CurveId if present
    if (original.FXVolatilities && !original.FXVolatilities->FXVolatility.empty()) {
        REQUIRE(!roundtripped.FXVolatilities->FXVolatility.empty());
        CHECK(std::string(roundtripped.FXVolatilities->FXVolatility[0].CurveId) ==
              std::string(original.FXVolatilities->FXVolatility[0].CurveId));
    }

    // Spot check: verify first swaption volatility CurveId if present
    if (original.SwaptionVolatilities && !original.SwaptionVolatilities->SwaptionVolatility.empty()) {
        REQUIRE(!roundtripped.SwaptionVolatilities->SwaptionVolatility.empty());
        CHECK(std::string(roundtripped.SwaptionVolatilities->SwaptionVolatility[0].CurveId) ==
              std::string(original.SwaptionVolatilities->SwaptionVolatility[0].CurveId));
    }
}

/**
 * @brief Perform a structural roundtrip test on curveconfiguration.
 */
void test_curveconfig_roundtrip(const std::string& xml_content,
                                 const std::string& source_name) {
    auto lg(make_logger(test_suite));

    // Step 1: Parse original XML
    BOOST_LOG_SEV(lg, debug) << "Parsing original XML from: " << source_name;
    curveconfiguration original;
    ores::ore::domain::load_data(xml_content, original);

    const std::size_t total_count = count_elements(original);
    BOOST_LOG_SEV(lg, debug) << "Parsed " << total_count << " total curve config elements";

    // Step 2: Serialize back to XML
    BOOST_LOG_SEV(lg, debug) << "Serializing to XML";
    const std::string serialized = ores::ore::domain::save_data(original);

    // Step 3: Parse the serialized XML
    BOOST_LOG_SEV(lg, debug) << "Parsing serialized XML";
    curveconfiguration roundtripped;
    ores::ore::domain::load_data(serialized, roundtripped);

    // Step 4: Compare domain objects
    BOOST_LOG_SEV(lg, debug) << "Comparing original and roundtripped objects";
    require_curveconfig_equal(original, roundtripped);

    BOOST_LOG_SEV(lg, info) << "Roundtrip test passed for: " << source_name;
}

void test_curveconfig_roundtrip_from_file(const std::string& relative_path) {
    const auto f = ore_path(relative_path);
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);
    test_curveconfig_roundtrip(content, f.string());
}

}

// =============================================================================
// File-Based Roundtrip Tests
// =============================================================================

TEST_CASE("curveconfig_roundtrip_main_xml", tags) {
    test_curveconfig_roundtrip_from_file("examples/Input/curveconfig.xml");
}

TEST_CASE("curveconfig_roundtrip_ore_api", tags) {
    test_curveconfig_roundtrip_from_file("examples/ORE-API/Input/curveconfig.xml");
}

TEST_CASE("curveconfig_roundtrip_inflation", tags) {
    test_curveconfig_roundtrip_from_file("examples/Exposure/Input/curveconfig_inflation.xml");
}

TEST_CASE("curveconfig_roundtrip_sabr", tags) {
    test_curveconfig_roundtrip_from_file("examples/CurveBuilding/Input/curveconfig_sabr.xml");
}

