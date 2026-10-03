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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/netting_set_mapper.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.ore.nettingsetdefinitions.mapper.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][nettingsetdefinitions][mapper]");

using ores::ore::domain::netting_set_mapper;
using ores::ore::domain::nettingsetdefinitions;
using namespace ores::logging;

std::vector<std::filesystem::path> netting_documents() {
    const auto root = ores::testing::project_root::resolve("external/ore/examples");
    std::vector<std::filesystem::path> r;
    for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;
        const auto content = ores::platform::filesystem::file::read_content(entry.path());
        if (content.find("<NettingSetDefinitions") != std::string::npos)
            r.push_back(entry.path());
    }
    return r;
}

nettingsetdefinitions parse(const std::string& xml) {
    nettingsetdefinitions r;
    ores::ore::domain::load_data(xml, r);
    return r;
}

// The binding types CSADetails as mixed content, so it keeps the text between
// its elements: in ORE's examples that is the whitespace left by elements
// commented out. It carries no data, so it is checked to be whitespace and
// then dropped before the documents are compared.
nettingsetdefinitions without_layout_text(nettingsetdefinitions v) {
    for (auto& n : v.NettingSet) {
        if (!n.CSADetails)
            continue;
        auto& layout = static_cast<std::string&>(*n.CSADetails);
        CHECK(layout.find_first_not_of(" \t\r\n") == std::string::npos);
        layout.clear();
    }
    return v;
}

}

TEST_CASE("every_ore_netting_document_roundtrips_through_the_entities", tags) {
    auto lg(make_logger(test_suite));

    const auto documents = netting_documents();
    REQUIRE(!documents.empty());

    std::size_t sets = 0;
    for (const auto& path : documents) {
        INFO(path.string());
        const auto original =
            parse(ores::platform::filesystem::file::read_content(path));
        const auto mapped = netting_set_mapper::map(original);
        CHECK(mapped.sets.size() == original.NettingSet.size());

        const auto expected = ores::ore::domain::save_data(without_layout_text(original));
        const auto actual = ores::ore::domain::save_data(netting_set_mapper::reverse(mapped));
        CHECK(actual == expected);
        sets += mapped.sets.size();
    }
    BOOST_LOG_SEV(lg, info) << "Round-tripped " << sets << " netting sets in "
                            << documents.size() << " documents.";
}

TEST_CASE("netting_set_details_carry_call_type_and_initial_margin_type", tags) {
    const std::string xml = R"(<NettingSetDefinitions>
  <NettingSet>
    <NettingSetDetails>
      <NettingSetId>CPTY_A</NettingSetId>
      <CallType>Call</CallType>
      <InitialMarginType>Schedule</InitialMarginType>
    </NettingSetDetails>
    <ActiveCSAFlag>false</ActiveCSAFlag>
  </NettingSet>
</NettingSetDefinitions>)";
    const auto original = parse(xml);

    const auto mapped = netting_set_mapper::map(original);
    REQUIRE(mapped.sets.size() == 1);
    CHECK(mapped.sets.front().code == "CPTY_A");
    CHECK(mapped.sets.front().call_type == "Call");
    CHECK(mapped.sets.front().initial_margin_type == "Schedule");
    CHECK(mapped.csas.empty());

    CHECK(ores::ore::domain::save_data(netting_set_mapper::reverse(mapped)) ==
          ores::ore::domain::save_data(original));
}

TEST_CASE("netting_set_details_naming_a_legal_entity_are_refused", tags) {
    const std::string xml = R"(<NettingSetDefinitions>
  <NettingSet>
    <NettingSetDetails>
      <NettingSetId>CPTY_A</NettingSetId>
      <LegalEntityId>BANK</LegalEntityId>
    </NettingSetDetails>
    <ActiveCSAFlag>false</ActiveCSAFlag>
  </NettingSet>
</NettingSetDefinitions>)";

    CHECK_THROWS_AS(netting_set_mapper::map(parse(xml)), std::runtime_error);
}

TEST_CASE("an_inactive_csa_keeps_its_terms", tags) {
    const auto path = ores::testing::project_root::resolve(
        "external/ore/examples/ExposureWithCollateral/Input/netting.xml");
    const auto mapped = netting_set_mapper::map(
        parse(ores::platform::filesystem::file::read_content(path)));

    std::size_t inactive = 0;
    for (const auto& c : mapped.csas) {
        if (!c.is_active) {
            ++inactive;
            CHECK(c.margin_period_of_risk.has_value());
        }
    }
    CHECK(inactive > 0);
}
