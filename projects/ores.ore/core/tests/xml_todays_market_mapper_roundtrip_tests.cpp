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
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/todays_market_mapper.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>

/**
 * @file xml_todays_market_mapper_roundtrip_tests.cpp
 * @brief The today's market document, mapped and mapped back.
 *
 * The walk is the proof over the corpus. The cases below it name the three
 * shapes the mapper had to special-case, so that a regression names the shape
 * rather than only the file.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][todaysmarket]");

std::filesystem::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

ores::ore::xml::roundtrip_kind todays_market_kind() {
    return ores::ore::xml::make_roundtrip_kind<ores::ore::domain::todaysmarket,
                                               ores::ore::domain::mapped_todays_market>(
        "today's market",
        "todaysmarket",
        &ores::ore::domain::todays_market_mapper::map,
        &ores::ore::domain::todays_market_mapper::reverse,
        ores::ore::xml::parsed_text_difference<ores::ore::domain::todaysmarket>);
}

}

TEST_CASE("every today's market document round trips through the entities", tags) {
    const auto walk = ores::ore::xml::walk_kind(todays_market_kind(), corpus_root());

    INFO("files: " << walk.files << ", passed: " << walk.passed);
    for (const auto& failure : walk.failures)
        INFO(failure);

    // The corpus holds ninety-eight documents of this kind, and the prefix
    // selects nothing else, so the walk should see all of them.
    CHECK(walk.files == 98);
    CHECK(walk.passed == walk.files);
}
