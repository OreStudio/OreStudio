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
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <map>
#include <sstream>
#include <string>

/**
 * @file xml_roundtrip_conventions_tests.cpp
 * @brief The conventions kind: what rounds trips, and what the mapper drops.
 *
 * The document carries twenty-six convention categories and the mapper models
 * nine. The files that use only those nine round trip. The rest do not, because
 * a category the mapper does not model is content the export cannot write, and
 * the measurement below says which categories those are and how many files each
 * one costs.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][conventions]");

using namespace ores::ore::domain;

std::filesystem::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

std::string read(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    std::ostringstream buffer;
    buffer << in.rdbuf();
    return buffer.str();
}

conventions load(const std::filesystem::path& path) {
    conventions document;
    load_data(read(path), document);
    return document;
}

ores::ore::xml::roundtrip_kind conventions_kind() {
    return ores::ore::xml::make_roundtrip_kind<conventions, mapped_conventions>(
        "conventions", "conventions", &conventions_mapper::map, &conventions_mapper::reverse,
        conventions_difference);
}

int element_count(const std::string& xml, const std::string& name) {
    const std::string open = "<" + name;
    int count = 0;
    std::size_t at = 0;
    while ((at = xml.find(open, at)) != std::string::npos) {
        const auto next = at + open.size();
        if (next < xml.size() && (xml[next] == '>' || xml[next] == ' ' || xml[next] == '/'))
            ++count;
        at = next;
    }
    return count;
}

}

// A category lands one at a time, and each one asserts its own elements survive
// the export. The whole kind cannot be asserted until every category has, so a
// case per category is how the work is proven as it goes.
TEST_CASE("conventions_swap_index_round_trips", tags) {
    int files_with_swap_index = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.SwapIndex.empty())
            continue;

        ++files_with_swap_index;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.swap_index.size() == document.SwapIndex.size());

        for (std::size_t i = 0; i < document.SwapIndex.size(); ++i) {
            const std::string id(document.SwapIndex[i].Id);
            CHECK(mapped.swap_index[i].id == id);
            CHECK(mapped.swap_index[i].conventions ==
                  std::string(document.SwapIndex[i].Conventions));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "SwapIndex");
        INFO(path.string() + ": the document has " +
             std::to_string(document.SwapIndex.size()) + " SwapIndex element(s), the export " +
             std::to_string(written));
        CHECK(written == static_cast<int>(document.SwapIndex.size()));
    }

    // Forty-seven of the seventy-two at the commit this was written at.
    CHECK(files_with_swap_index == 47);
}

TEST_CASE("conventions_future_round_trips", tags) {
    int files_with_future = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.Future.empty())
            continue;

        ++files_with_future;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.future.size() == document.Future.size());

        for (std::size_t i = 0; i < document.Future.size(); ++i) {
            const std::string id(document.Future[i].Id);
            CHECK(mapped.future[i].id == id);
            CHECK(mapped.future[i].index == std::string(document.Future[i].Index));
            if (document.Future[i].DateGenerationRule)
                CHECK(mapped.future[i].date_generation_rule ==
                      to_string(*document.Future[i].DateGenerationRule));
            if (document.Future[i].OvernightIndexFutureNettingType)
                CHECK(mapped.future[i].netting_type ==
                      to_string(*document.Future[i].OvernightIndexFutureNettingType));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "Future");
        INFO(path.string() + ": the document has " + std::to_string(document.Future.size()) +
             " Future element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.Future.size()));
    }

    // Thirty-nine of the seventy-two at the commit this was written at.
    CHECK(files_with_future == 39);
}

TEST_CASE("conventions_fx_option_round_trips", tags) {
    int files_with_fx_option = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.FxOption.empty())
            continue;

        ++files_with_fx_option;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.fx_option.size() == document.FxOption.size());

        for (std::size_t i = 0; i < document.FxOption.size(); ++i) {
            CHECK(mapped.fx_option[i].id == std::string(document.FxOption[i].Id));
            CHECK(mapped.fx_option[i].atm_type == std::string(document.FxOption[i].AtmType));
            CHECK(mapped.fx_option[i].delta_type == std::string(document.FxOption[i].DeltaType));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "FxOption");
        INFO(path.string() + ": the document has " + std::to_string(document.FxOption.size()) +
             " FxOption element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.FxOption.size()));
    }

    // Thirty-six of the seventy-two at the commit this was written at.
    CHECK(files_with_fx_option == 36);
}

TEST_CASE("conventions_files_using_only_modelled_categories_round_trip", tags) {
    const auto kind = conventions_kind();
    int walked = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        // A file that carries a category the mapper does not model cannot round
        // trip, and saying which those are is the measurement's job rather than
        // this case's.
        if (!conventions_mapper::map(load(path)).unmodelled.empty())
            continue;

        ++walked;
        const auto outcome = kind.check(path);
        INFO(outcome.detail);
        CHECK(outcome.passed);
    }

    // Twelve of the seventy-two once SwapIndex and Future were modelled, and
    // nine before. The count is asserted so that a change in it is noticed
    // rather than absorbed.
    CHECK(walked == 12);
}

// Hidden by default, and run on demand:
//   ores.ore.core.tests "[.][conventions]"
// The categories below are the ones the corpus needs, ordered by how many files
// use them. Modelling starts at the top of that list.
TEST_CASE("conventions_unmodelled_categories_measurement", "[.][conventions][measurement]") {
    std::map<std::string, int> files_by_category;
    std::map<std::string, int> elements_by_category;
    int files = 0;
    int files_with_unmodelled = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto mapped = conventions_mapper::map(load(path));
        ++files;
        if (!mapped.unmodelled.empty())
            ++files_with_unmodelled;
        for (const auto& [name, count] : mapped.unmodelled) {
            ++files_by_category[name];
            elements_by_category[name] += static_cast<int>(count);
        }
    }

    WARN("conventions files=" + std::to_string(files) +
         " with unmodelled categories=" + std::to_string(files_with_unmodelled));
    for (const auto& [name, count] : files_by_category) {
        WARN("  " + name + " in " + std::to_string(count) + " file(s), " +
             std::to_string(elements_by_category[name]) + " element(s)");
    }

    CHECK(files == 72);
}
