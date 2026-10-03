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
#include "ores.analytics.core/repository/stress_test_library_repository.hpp"
#include "ores.analytics.core/repository/stress_test_scenario_repository.hpp"
#include "ores.analytics.core/repository/stress_test_shift_repository.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/stress_test_mapper.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <algorithm>
#include <set>
#include <string>

/**
 * @file stress_test_database_roundtrip_tests.cpp
 * @brief The stress test document through the database.
 *
 * The document is written with the shift family and shift type each shift
 * names, which the database checks against the seeded vocabularies, then read
 * back and turned into a document again, which must equal the mapper's own
 * round trip of the same file. The chosen file applies shifts in ten
 * of the sixteen families and uses all three shift types, so the round trip
 * also shows the vocabularies accept what the corpus writes.
 */

namespace {

const std::string tags("[stresstest][database][roundtrip]");

using namespace ores::ore::domain;
using namespace ores::analytics::repository;

const std::string example = "external/ore/examples/MarketRisk/Input/stresstest.xml";

template <typename Row>
void stamp(Row& r) {
    r.modified_by = "ores";
    r.performed_by = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary = "Imported from ORE XML";
}

// The mapper turns a document into rows and leaves identity and provenance to
// whoever stores them, so the test supplies both before writing.
mapped_stress_test persistable(mapped_stress_test m) {
    boost::uuids::random_generator next;
    m.library.id = next();
    if (m.library.name.empty())
        m.library.name = "stresstest";
    stamp(m.library);
    for (auto& s : m.scenarios) {
        s.scenario.id = next();
        s.scenario.stress_test_library_id = m.library.id;
        stamp(s.scenario);
        for (auto& shift : s.shifts) {
            shift.id = next();
            shift.stress_test_scenario_id = s.scenario.id;
            stamp(shift);
        }
    }
    return m;
}

const mapped_stress_scenario& first_with_shifts(const mapped_stress_test& m) {
    for (const auto& s : m.scenarios)
        if (!s.shifts.empty())
            return s;
    FAIL("no scenario of the example applies a shift");
    return m.scenarios.front();
}

stresstesting load_example() {
    stresstesting d;
    load_data(ores::platform::filesystem::file::read_content(
                  ores::testing::project_root::resolve(example)),
              d);
    return d;
}

}

TEST_CASE("stress_test_roundtrip_through_the_database", tags) {
    ores::testing::scoped_database_helper h;
    const auto original = load_example();
    const auto mapped = persistable(stress_test_mapper::map(original));
    REQUIRE(!mapped.scenarios.empty());

    std::set<std::string> families;
    std::set<std::string> types;
    stress_test_library_repository().write(h.context(), mapped.library);
    for (const auto& s : mapped.scenarios) {
        stress_test_scenario_repository().write(h.context(), s.scenario);
        stress_test_shift_repository().write(h.context(), s.shifts);
        for (const auto& shift : s.shifts) {
            families.insert(shift.family);
            if (shift.shift_type)
                types.insert(*shift.shift_type);
        }
    }
    CHECK(families.size() == 10);
    CHECK(types.size() == 3);

    mapped_stress_test back;
    back.library = mapped.library;
    for (const auto& scenario : stress_test_scenario_repository().read_latest(h.context())) {
        if (scenario.stress_test_library_id != mapped.library.id)
            continue;
        mapped_stress_scenario s;
        s.scenario = scenario;
        for (const auto& shift : stress_test_shift_repository().read_latest(h.context()))
            if (shift.stress_test_scenario_id == scenario.id)
                s.shifts.push_back(shift);
        std::sort(s.shifts.begin(), s.shifts.end(), [](const auto& l, const auto& r) {
            return l.position < r.position;
        });
        back.scenarios.push_back(std::move(s));
    }
    std::sort(back.scenarios.begin(), back.scenarios.end(), [](const auto& l, const auto& r) {
        return l.scenario.position < r.scenario.position;
    });
    REQUIRE(back.scenarios.size() == mapped.scenarios.size());

    // The mapper writes an empty family element back as no element, which its
    // own round trip documents, so the database result is compared with the
    // mapper's in-memory result: what the tables must hold is what the mapper
    // produces.
    stresstesting in_memory;
    load_data(save_data(stress_test_mapper::reverse(stress_test_mapper::map(original))), in_memory);
    stresstesting exported;
    load_data(save_data(stress_test_mapper::reverse(back)), exported);
    const auto difference = ores::ore::xml::parsed_text_difference(in_memory, exported, example);
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("a shift naming a family ORE does not have is refused", tags) {
    ores::testing::scoped_database_helper h;
    const auto mapped = persistable(stress_test_mapper::map(load_example()));
    const auto& scenario = first_with_shifts(mapped);
    auto shift = scenario.shifts.front();
    shift.family = "DiscountCurve";

    stress_test_library_repository().write(h.context(), mapped.library);
    stress_test_scenario_repository().write(h.context(), scenario.scenario);
    CHECK_THROWS(stress_test_shift_repository().write(h.context(), shift));
}

TEST_CASE("a shift naming a shift type ORE does not have is refused", tags) {
    ores::testing::scoped_database_helper h;
    const auto mapped = persistable(stress_test_mapper::map(load_example()));
    const auto& scenario = first_with_shifts(mapped);
    auto shift = scenario.shifts.front();
    shift.shift_type = "Proportional";

    stress_test_library_repository().write(h.context(), mapped.library);
    stress_test_scenario_repository().write(h.context(), scenario.scenario);
    CHECK_THROWS(stress_test_shift_repository().write(h.context(), shift));
}
