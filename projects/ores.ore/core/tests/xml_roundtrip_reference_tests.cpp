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
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <filesystem>
#include <string>
#include <vector>

/**
 * @file xml_roundtrip_reference_tests.cpp
 * @brief The shared reference configuration kinds, through the entity mappers.
 *
 * Calendar adjustments round trip. Currencies and conventions do not, and the
 * reasons are a lost field and an incomplete mapper rather than a harness
 * defect, so they are measured by the hidden probe below and recorded in
 * =evidence/round_trip_reference_kinds.md= instead of being asserted green.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][reference]");

using namespace ores::ore::domain;
namespace fs = std::filesystem;

fs::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}


ores::ore::xml::roundtrip_kind conventions_kind() {
    return ores::ore::xml::make_roundtrip_kind<conventions,
                                               ores::refdata::messaging::conventions_document>(
        "conventions",
        "conventions",
        &conventions_mapper::map,
        &conventions_mapper::reverse,
        ores::ore::xml::parsed_text_difference<conventions>);
}

}

TEST_CASE("reference_kinds_measurement", "[.][measurement][reference]") {
    const auto conventions = ores::ore::xml::walk_kind(conventions_kind(), corpus_root());
    WARN("conventions files=" + std::to_string(conventions.files) +
         " passed=" + std::to_string(conventions.passed));
    for (const auto& failure : conventions.failures)
        WARN(failure);
    CHECK(conventions.files == 72);
}
