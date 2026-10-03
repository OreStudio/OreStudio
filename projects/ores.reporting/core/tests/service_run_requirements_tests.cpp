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
#include "ores.reporting.core/service/run_requirements.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

// The decision these pin: which required configuration types a definition
// fails to bind. The trigger refuses a run on a non-empty answer, so a wrong
// answer either starts a run that cannot finish or refuses one that could.

using namespace ores::reporting::service;

namespace {

const std::string tags("[service][run_requirements]");

const std::vector<std::string> risk_requires{
    "pricing_engines", "todays_market", "curve_configuration", "conventions"};

}

TEST_CASE("a definition binding every required type misses nothing", tags) {
    const std::vector<std::string> bound{
        "conventions", "curve_configuration", "todays_market", "pricing_engines"};
    CHECK(missing_configuration_types(risk_requires, bound).empty());
}

TEST_CASE("extra bound types do not count as missing", tags) {
    const std::vector<std::string> bound{"pricing_engines",
                                         "todays_market",
                                         "curve_configuration",
                                         "conventions",
                                         "simulation",
                                         "sensitivity"};
    CHECK(missing_configuration_types(risk_requires, bound).empty());
}

TEST_CASE("a definition binding nothing misses every required type, sorted", tags) {
    const std::vector<std::string> expected{
        "conventions", "curve_configuration", "pricing_engines", "todays_market"};
    CHECK(missing_configuration_types(risk_requires, {}) == expected);
}

TEST_CASE("only the unbound required types are reported", tags) {
    const std::vector<std::string> bound{"pricing_engines", "conventions", "simulation"};
    const std::vector<std::string> expected{"curve_configuration", "todays_market"};
    CHECK(missing_configuration_types(risk_requires, bound) == expected);
}

TEST_CASE("a requirement stated twice is reported once", tags) {
    const std::vector<std::string> required{"conventions", "conventions"};
    const std::vector<std::string> expected{"conventions"};
    CHECK(missing_configuration_types(required, {}) == expected);
}

TEST_CASE("a report type with no requirements misses nothing", tags) {
    CHECK(missing_configuration_types({}, {"pricing_engines"}).empty());
}
