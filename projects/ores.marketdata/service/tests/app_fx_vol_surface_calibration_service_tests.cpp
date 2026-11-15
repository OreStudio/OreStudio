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
#include "ores.marketdata.service/app/fx_vol_surface_calibration_service.hpp"
#include "ores.testing/uuid_of.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>

namespace {

using ores::marketdata::service::app::calibration_id_of;
using ores::testing::uuid_of;

/// Two blueprints, named so the unity build cannot confuse them with the
/// instruments other suites in this component call eur_usd and gbp_usd.
const boost::uuids::uuid eur_usd_blueprint = uuid_of("00000000-0000-4000-8000-0000000000a1");
const boost::uuids::uuid gbp_usd_blueprint = uuid_of("00000000-0000-4000-8000-0000000000a2");

/// One calibration instant, well inside the years the fixtures use.
const auto calibration_at =
    std::chrono::system_clock::time_point{std::chrono::sys_days{std::chrono::year_month_day{
        std::chrono::year{2026}, std::chrono::month{11}, std::chrono::day{9}}}} +
    std::chrono::hours{10};

}

TEST_CASE("a calibration is named by its blueprint and its as-of",
          "[app][fx_vol_surface_calibration]") {
    // A repeat of the same request names the same calibration, so its grid and
    // its lineage are the rows the first attempt wrote.
    CHECK(calibration_id_of(eur_usd_blueprint, calibration_at) ==
          calibration_id_of(eur_usd_blueprint, calibration_at));
    // Another blueprint at the same instant is another calibration.
    CHECK(calibration_id_of(eur_usd_blueprint, calibration_at) !=
          calibration_id_of(gbp_usd_blueprint, calibration_at));
    // A later instant is a new calibration, not a repeat of this one.
    CHECK(calibration_id_of(eur_usd_blueprint, calibration_at) !=
          calibration_id_of(eur_usd_blueprint, calibration_at + std::chrono::seconds{1}));
    // The id says which calibration, so it is never the empty one.
    CHECK(calibration_id_of(eur_usd_blueprint, calibration_at) != boost::uuids::nil_uuid());
}
