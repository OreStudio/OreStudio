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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.iam.core/messaging/party_surface_hold_back.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

using ores::iam::messaging::party_surface_hold_back;
using ores::marketdata::messaging::calibrate_fx_vol_surface_response;
using ores::marketdata::messaging::fx_vol_pillar_fit;

calibrate_fx_vol_surface_response clean() {
    calibrate_fx_vol_surface_response out;
    out.success = true;
    out.converged = true;
    out.pillars.push_back(fx_vol_pillar_fit{.point = "1M ATM", .error = 0.0001});
    return out;
}

}

TEST_CASE("a clean calibration is published", "[party_surface_hold_back]") {
    CHECK(!party_surface_hold_back(clean()));
}

TEST_CASE("a calibration with arbitrage is held back, naming each condition",
          "[party_surface_hold_back]") {
    auto c = clean();
    c.violations.push_back({.kind = "butterfly", .expiry = "3M", .description = "g < 0"});
    const auto reason = party_surface_hold_back(c);
    REQUIRE(reason);
    CHECK(reason->find("1 arbitrage condition(s): butterfly at 3M;") != std::string::npos);
}

TEST_CASE("a fit that did not converge is held back", "[party_surface_hold_back]") {
    auto c = clean();
    c.converged = false;
    CHECK(party_surface_hold_back(c) == std::optional<std::string>("the fit did not converge"));
}

TEST_CASE("a pillar beyond the tolerance is held back, counting the pillars",
          "[party_surface_hold_back]") {
    auto c = clean();
    c.pillars.push_back(
        fx_vol_pillar_fit{.point = "1Y 10P", .error = 0.02, .beyond_tolerance = true});
    c.pillars.push_back(
        fx_vol_pillar_fit{.point = "1Y 10C", .error = 0.03, .beyond_tolerance = true});
    c.max_abs_error = 0.03;
    const auto reason = party_surface_hold_back(c);
    REQUIRE(reason);
    CHECK(reason->starts_with("2 pillar(s) are beyond the tolerance"));
}
