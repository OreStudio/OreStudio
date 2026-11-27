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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#include "ores.ore.core/domain/calendar_names.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

constexpr const char* calendar_name_tags = "[calendar_names]";

namespace {

using ores::ore::domain::ore_calendar_name;

}

TEST_CASE("a calendar ORE reads under another name is renamed", calendar_name_tags) {
    CHECK(ore_calendar_name("UnitedKingdom.Settlement") == "UK settlement");
    CHECK(ore_calendar_name("UnitedKingdom.Exchange") == "London stock exchange");
    CHECK(ore_calendar_name("UnitedStates.Settlement") == "US settlement");
    CHECK(ore_calendar_name("UnitedStates.SOFR") == "SOFR fixing calendar");
}

TEST_CASE("a calendar the two vocabularies spell alike is unchanged", calendar_name_tags) {
    CHECK(ore_calendar_name("TARGET") == "TARGET");
    CHECK(ore_calendar_name("Australia settlement") == "Australia settlement");
}

TEST_CASE("an empty code is returned as it came", calendar_name_tags) {
    CHECK(ore_calendar_name("") == "");
}
