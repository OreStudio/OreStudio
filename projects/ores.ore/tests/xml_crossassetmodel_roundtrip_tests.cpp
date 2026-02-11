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

const std::string_view test_suite("ores.ore.crossassetmodel.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][crossassetmodel]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using ores::ore::domain::crossAssetModel;
using namespace ores::logging;

void require_crossassetmodel_equal(const crossAssetModel& original,
                                    const crossAssetModel& roundtripped) {
    CHECK(roundtripped.DomesticCcy == original.DomesticCcy);
    CHECK(roundtripped.Currencies.Currency.size() ==
          original.Currencies.Currency.size());
    CHECK(roundtripped.BootstrapTolerance == original.BootstrapTolerance);
    CHECK(roundtripped.InterestRateModels.LGM.size() ==
          original.InterestRateModels.LGM.size());
    CHECK(roundtripped.InterestRateModels.HWModel.size() ==
          original.InterestRateModels.HWModel.size());
    CHECK(static_cast<bool>(roundtripped.ForeignExchangeModels) ==
          static_cast<bool>(original.ForeignExchangeModels));
    if (original.ForeignExchangeModels)
        CHECK(roundtripped.ForeignExchangeModels->CrossCcyLGM.size() ==
              original.ForeignExchangeModels->CrossCcyLGM.size());
    CHECK(static_cast<bool>(roundtripped.InstantaneousCorrelations) ==
          static_cast<bool>(original.InstantaneousCorrelations));
    if (original.InstantaneousCorrelations)
        CHECK(roundtripped.InstantaneousCorrelations->Correlation.size() ==
              original.InstantaneousCorrelations->Correlation.size());
}

void test_roundtrip_from_file(const std::string& relative_path) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path(relative_path);
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    BOOST_LOG_SEV(lg, debug) << "Parsing original XML from: " << f.string();
    crossAssetModel original;
    ores::ore::domain::load_data(content, original);

    BOOST_LOG_SEV(lg, debug) << "Serializing to XML";
    const std::string serialized = ores::ore::domain::save_data(original);

    BOOST_LOG_SEV(lg, debug) << "Parsing serialized XML";
    crossAssetModel roundtripped;
    ores::ore::domain::load_data(serialized, roundtripped);

    require_crossassetmodel_equal(original, roundtripped);
    BOOST_LOG_SEV(lg, info) << "Roundtrip test passed for: " << f.string();
}

}

TEST_CASE("crossassetmodel_roundtrip_hw_historical_calibration", tags) {
    test_roundtrip_from_file(
        "examples/Exposure/ExpectedOutput/hwhistoricalcalibration/calibration.xml");
}

TEST_CASE("crossassetmodel_roundtrip_hw_historical_calibration_pca", tags) {
    test_roundtrip_from_file(
        "examples/Exposure/ExpectedOutput/hwhistoricalcalibration_pca/calibration.xml");
}
