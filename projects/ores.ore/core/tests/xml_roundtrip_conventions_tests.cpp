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
#include <iostream>
#include <map>
#include <sstream>
#include <string>

/**
 * @file xml_roundtrip_conventions_tests.cpp
 * @brief The conventions kind: what the mapper models and what it drops.
 *
 * The document carries twenty-six convention categories and the mapper models
 * nine of them. Before the mapper counted what it skipped, none of the
 * seventy-two corpus files round tripped and nothing said which categories the
 * documents actually use, so the work of closing the gap could not be scoped.
 * The measurement below is that scoping: it reports, per category, how many
 * files carry it.
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

// The kind is built here rather than shared with the reference suite, because
// the two suites answer different questions: that one registers the kinds that
// round trip, this one measures the kind that does not yet.
ores::ore::xml::roundtrip_kind conventions_kind() {
    return ores::ore::xml::make_roundtrip_kind<conventions, mapped_conventions>(
        "conventions", "conventions", &conventions_mapper::map, &conventions_mapper::reverse,
        ores::ore::xml::parsed_text_difference<conventions>);
}

/**
 * @brief The distinct element names a document carries.
 *
 * The round-trip comparison says where two documents first disagree, which is
 * one field at a time. This says which fields the export dropped across a whole
 * corpus in one pass, which is the list the work needs. Comments are skipped so
 * that a tag written inside one is not read as an element.
 */
std::map<std::string, int> element_counts(const std::string& xml) {
    std::map<std::string, int> names;
    std::size_t at = 0;
    while ((at = xml.find('<', at)) != std::string::npos) {
        if (xml.compare(at, 4, "<!--") == 0) {
            const auto end = xml.find("-->", at);
            at = end == std::string::npos ? xml.size() : end + 3;
            continue;
        }
        if (at + 1 < xml.size() && (xml[at + 1] == '?' || xml[at + 1] == '!')) {
            const auto end = xml.find('>', at);
            at = end == std::string::npos ? xml.size() : end + 1;
            continue;
        }
        std::size_t name_at = at + 1;
        if (name_at < xml.size() && xml[name_at] == '/')
            ++name_at;
        const auto end = xml.find_first_of(" \t\r\n/>", name_at);
        if (end == std::string::npos) {
            at = xml.size();
            continue;
        }
        if (end > name_at)
            ++names[xml.substr(name_at, end - name_at)];
        at = end;
    }
    return names;
}

}

// Hidden by default, and run on demand:
//   ores.ore.core.tests "[.][conventions]"
// Nine of the seventy-two files carry only categories the mapper models. They
// are the ones a field mapping can fix, so their first differences are the list
// of fields the modelled categories are missing.
TEST_CASE("conventions_modelled_only_files_measurement", "[.][conventions][measurement]") {
    const auto kind = conventions_kind();
    std::map<std::string, int> missing_by_element;
    int clean = 0;
    int passing = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const std::string content = read(path);
        conventions document;
        load_data(content, document);

        const auto mapped = conventions_mapper::map(document);
        if (!mapped.unmodelled.empty())
            continue;

        ++clean;
        const auto outcome = kind.check(path);
        if (outcome.passed)
            ++passing;
        else
            std::cout << "\n" << outcome.detail << "\n";

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const auto imported_counts = element_counts(content);
        const auto exported_counts = element_counts(exported);
        for (const auto& [name, count] : imported_counts) {
            const auto found = exported_counts.find(name);
            const int exported_count = found == exported_counts.end() ? 0 : found->second;
            if (exported_count < count)
                ++missing_by_element[name];
        }
    }

    std::cout << "\nconventions files using only modelled categories=" << clean
              << " passing=" << passing << "\n";
    for (const auto& [element, count] : missing_by_element)
        std::cout << "  fewer <" << element << "> in " << count << " file(s)\n";
    CHECK(clean > 0);
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
        conventions document;
        load_data(read(path), document);

        const auto mapped = conventions_mapper::map(document);
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
