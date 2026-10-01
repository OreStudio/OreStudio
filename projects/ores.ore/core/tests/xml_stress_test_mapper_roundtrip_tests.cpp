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
#include "ores.ore.core/domain/stress_test_mapper.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <map>
#include <sstream>
#include <string>

/**
 * @file xml_stress_test_mapper_roundtrip_tests.cpp
 * @brief The stress document's library and scenarios, mapped and mapped back.
 *
 * The shift blocks a scenario applies are not modelled yet, so each family a
 * scenario uses is counted. The case compares what is modelled -- the library's
 * setting and every scenario's name, date and place -- and reports the counted
 * families, so that nothing is dropped without being named.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][stress]");

std::filesystem::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

std::string read(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    std::ostringstream buffer;
    buffer << in.rdbuf();
    return buffer.str();
}

ores::ore::domain::stresstesting load(const std::filesystem::path& path) {
    ores::ore::domain::stresstesting document;
    ores::ore::domain::load_data(read(path), document);
    return document;
}

}

TEST_CASE("stress_test_library_and_scenarios_round_trip_over_the_corpus", tags) {
    using namespace ores::ore::domain;

    int files = 0;
    int scenarios = 0;
    std::map<std::string, int> families_in_use;

    for (const auto& path : ores::ore::xml::files_of_kind("stress", corpus_root())) {
        ++files;
        const auto document = load(path);

        const auto mapped = stress_test_mapper::map(document);
        REQUIRE(mapped.scenarios.size() == document.StressTest.size());
        if (document.UseSpreadedTermStructures)
            REQUIRE(mapped.library.use_spreaded_term_structures.has_value());

        for (const auto& [family, count] : mapped.unmodelled)
            families_in_use[family] += static_cast<int>(count);

        const auto reversed = stress_test_mapper::reverse(mapped);
        REQUIRE(reversed.StressTest.size() == document.StressTest.size());

        for (std::size_t i = 0; i < mapped.scenarios.size(); ++i) {
            ++scenarios;
            const auto& in = document.StressTest.at(i);
            const auto& out = reversed.StressTest.at(i);
            const auto& row = mapped.scenarios.at(i);

            INFO(path.string() + ": scenario " + std::to_string(i));
            CHECK(row.scenario.name == in.id);
            CHECK(out.id == in.id);
            CHECK(row.scenario.position == static_cast<int>(i) + 1);
            CHECK(static_cast<bool>(out.Date) == static_cast<bool>(in.Date));
            if (in.Date)
                CHECK(std::string(*out.Date) == std::string(*in.Date));

            // The DiscountCurves shifts the scenario applies, which are the
            // first family to be mapped.
            const std::size_t expected =
                in.DiscountCurves ? in.DiscountCurves->DiscountCurve.size() : 0;
            REQUIRE(row.shifts.size() == expected);
            // An empty block means the same as no block, so one that carries
            // no entries is written back as none. The corpus has such a block.
            if (expected == 0) {
                if (out.DiscountCurves)
                    CHECK(out.DiscountCurves->DiscountCurve.empty());
            } else {
                REQUIRE(static_cast<bool>(out.DiscountCurves));
            }
            for (std::size_t j = 0; j < expected; ++j) {
                const auto& original = in.DiscountCurves->DiscountCurve.at(j);
                const auto& stored = row.shifts.at(j);
                const auto& round_tripped = out.DiscountCurves->DiscountCurve.at(j);
                CHECK(stored.family == "DiscountCurves");
                CHECK(stored.object_key == to_string(original.ccy));
                CHECK(round_tripped.ccy == original.ccy);
                CHECK(static_cast<bool>(round_tripped.Shifts) ==
                      static_cast<bool>(original.Shifts));
                if (original.Shifts)
                    CHECK(std::string(*round_tripped.Shifts) == std::string(*original.Shifts));
            }
        }

        CHECK(static_cast<bool>(reversed.UseSpreadedTermStructures) ==
              static_cast<bool>(document.UseSpreadedTermStructures));
    }

    CHECK(files == 20);
    CHECK(scenarios == 46);

    // How many of the corpus's forty-six scenarios apply each of the four
    // busiest families. They are counted because the shifts are not modelled,
    // and asserted so that the gap is measured rather than assumed.
    CHECK(families_in_use.at("FxSpots") == 41);
    CHECK(families_in_use.at("IndexCurves") == 41);
    CHECK(families_in_use.at("FxVolatilities") == 41);
}

// The cases the corpus does not carry: a scenario with no date, a document with
// no scenarios at all, and the library setting absent, true and false. Each is
// built by hand because the corpus has none of them.
TEST_CASE("stress_test_mapper_keeps_the_shapes_the_corpus_lacks", tags) {
    using namespace ores::ore::domain;

    const auto round_trip = [](const std::string& xml) {
        stresstesting original;
        load_data(xml, original);
        const auto mapped = stress_test_mapper::map(original);
        const auto reversed = stress_test_mapper::reverse(mapped);
        return std::make_pair(original, reversed);
    };

    SECTION("a scenario with no date keeps no date") {
        const auto [original, reversed] = round_trip(R"(<StressTesting>
  <StressTest id="theta">
    <DiscountCurves/>
  </StressTest>
</StressTesting>)");
        REQUIRE(reversed.StressTest.size() == 1);
        CHECK(reversed.StressTest.front().id == "theta");
        CHECK(static_cast<bool>(reversed.StressTest.front().Date) ==
              static_cast<bool>(original.StressTest.front().Date));
        CHECK(!reversed.StressTest.front().Date);
    }

    // A document with no scenarios at all cannot be built: ORE's schema makes
    // StressTest occur at least once, and the binding refuses it. The case says
    // so rather than pretending the shape exists.
    SECTION("a document with no scenarios is refused by the schema") {
        stresstesting empty;
        CHECK_THROWS(load_data("<StressTesting></StressTesting>", empty));
    }

    SECTION("the library setting is absent, and stays absent") {
        const auto [original, reversed] =
            round_trip("<StressTesting><StressTest id=\"theta\"/></StressTesting>");
        CHECK(!reversed.UseSpreadedTermStructures);
    }

    // The binding gives each spelling of a boolean its own enumerator, so true
    // written back as True is a different enumerator holding the same value.
    // The comparison is the mapper's own: map the reversal and compare that.
    SECTION("the library setting is true, and stays true") {
        const auto xml = std::string("<StressTesting><UseSpreadedTermStructures>true"
                                     "</UseSpreadedTermStructures><StressTest id=\"theta\"/>"
                                     "</StressTesting>");
        stresstesting original;
        load_data(xml, original);
        const auto mapped = stress_test_mapper::map(original);
        REQUIRE(mapped.library.use_spreaded_term_structures.has_value());
        CHECK(*mapped.library.use_spreaded_term_structures);

        const auto reversals = stress_test_mapper::map(stress_test_mapper::reverse(mapped));
        CHECK(reversals.library.use_spreaded_term_structures ==
              mapped.library.use_spreaded_term_structures);
    }

    SECTION("the library setting is false, and stays false") {
        const auto xml = std::string("<StressTesting><UseSpreadedTermStructures>false"
                                     "</UseSpreadedTermStructures><StressTest id=\"theta\"/>"
                                     "</StressTesting>");
        stresstesting original;
        load_data(xml, original);
        const auto mapped = stress_test_mapper::map(original);
        REQUIRE(mapped.library.use_spreaded_term_structures.has_value());
        CHECK(!*mapped.library.use_spreaded_term_structures);

        const auto reversals = stress_test_mapper::map(stress_test_mapper::reverse(mapped));
        CHECK(reversals.library.use_spreaded_term_structures ==
              mapped.library.use_spreaded_term_structures);
    }
}
