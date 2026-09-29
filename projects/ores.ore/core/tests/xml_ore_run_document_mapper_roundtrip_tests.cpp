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
#include "ores.ore.core/domain/run_document_mapper.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <map>
#include <sstream>
#include <string>

/**
 * @file xml_ore_run_document_mapper_roundtrip_tests.cpp
 * @brief The run document's Setup block, mapped to the setup entity and back.
 *
 * Every shipped run document is walked: its Setup parameters are read into
 * report_run_setup, the entity is written back out as an ORE parameter list,
 * and the two parameter sets are compared name for name. The entity holds the
 * values as ORE spells them, so nothing is expected to change on the way
 * through and the comparison is exact.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][ore][run_document]");

std::filesystem::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

std::string read(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    std::ostringstream buffer;
    buffer << in.rdbuf();
    return buffer.str();
}

ores::ore::domain::ore load(const std::filesystem::path& path) {
    ores::ore::domain::ore document;
    ores::ore::domain::load_data(read(path), document);
    return document;
}

std::map<std::string, std::string> parameters_of(
    const ores::ore::domain::parameterListType& list) {
    std::map<std::string, std::string> out;
    for (const auto& parameter : list.Parameter)
        out[std::string(parameter.name)] = static_cast<const std::string&>(parameter);
    return out;
}

}

TEST_CASE("ore_run_document_setup_round_trips_over_the_corpus", tags) {
    using namespace ores::ore::domain;

    int files = 0;
    int ignored_parameters = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("ore", corpus_root())) {
        ++files;
        const auto document = load(path);
        const auto original = parameters_of(document.Setup);

        const auto setup = run_document_mapper::map_setup(document);
        const auto reversed = run_document_mapper::reverse_setup(setup);
        const auto exported = parameters_of(reversed);

        INFO(path.string() + ": the document has " + std::to_string(original.size()) +
             " Setup parameter(s), the export " + std::to_string(exported.size()));
        REQUIRE(exported.size() == original.size());

        for (const auto& [name, value] : original) {
            const auto it = exported.find(name);
            if (it == exported.end()) {
                INFO(path.string() + ": the export is missing " + name);
                CHECK(false);
                continue;
            }
            INFO(path.string() + ": " + name);
            CHECK(it->second == value);
        }

        // Every parameter the document writes must have a column on the entity,
        // or the export would be missing it and the count above would say so.
        for (const auto& [name, value] : original) {
            if (!exported.contains(name))
                ++ignored_parameters;
        }
    }

    // The corpus holds four hundred and sixteen run documents.
    CHECK(files == 416);
    CHECK(ignored_parameters == 0);
}
