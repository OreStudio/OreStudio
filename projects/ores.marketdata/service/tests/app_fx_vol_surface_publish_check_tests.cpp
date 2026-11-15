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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.marketdata.service/app/fx_vol_surface_publish_service.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

using ores::marketdata::domain::fx_vol_surface_calibration;
using ores::marketdata::service::app::blocking_failure_of;

fx_vol_surface_calibration calibrated(std::string violations) {
    fx_vol_surface_calibration out;
    out.converged = true;
    out.violations = std::move(violations);
    return out;
}

}

TEST_CASE("a calibration with no arbitrage may be published", "[app][fx_vol_surface_publish]") {
    CHECK(!blocking_failure_of(calibrated("[]")));
}

TEST_CASE("a pillar beyond the tolerance is the decider's to weigh, not a refusal",
          "[app][fx_vol_surface_publish]") {
    auto calibration = calibrated("[]");
    calibration.pillars_beyond_tolerance = 3;
    calibration.max_abs_error = 0.05;
    CHECK(!blocking_failure_of(calibration));
}

TEST_CASE("a calibration with arbitrage is refused under any rule, naming each condition",
          "[app][fx_vol_surface_publish]") {
    const auto failure = blocking_failure_of(
        calibrated(R"([{"kind":"butterfly","expiry":"3M","description":"g < 0"},)"
                   R"({"kind":"calendar","expiry":"1Y","description":"total variance falls"}])"));
    REQUIRE(failure);
    CHECK(failure->find("2 arbitrage condition(s)") != std::string::npos);
    CHECK(failure->find("butterfly at 3M") != std::string::npos);
    CHECK(failure->find("calendar at 1Y") != std::string::npos);
}

TEST_CASE("a calibration whose checks cannot be read is refused", "[app][fx_vol_surface_publish]") {
    CHECK(blocking_failure_of(calibrated("not json")));
    CHECK(blocking_failure_of(calibrated("")));
}
