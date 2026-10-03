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
#include "ores.analytics.core/repository/todays_market_collection_kind_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <catch2/catch_test_macros.hpp>
#include <fstream>
#include <iterator>
#include <map>
#include <regex>
#include <string>
#include <tuple>
#include <vector>

/**
 * @file repository_collection_kind_seed_tests.cpp
 * @brief The seeded today's market collection kinds against the ORE schema.
 *
 * Each kind, its entry element and its key attributes are read out of
 * todaysmarket.xsd, so a seed that drops, adds or misspells any of them fails
 * here rather than when a document is imported.
 */

namespace {

const std::string tags("[repository][collection_kinds][database]");

using kind_facts = std::tuple<std::string, std::string, std::string>;

std::string read_schema() {
    std::ifstream in(ores::testing::project_root::resolve("external/ore/xsd/todaysmarket.xsd"));
    REQUIRE(in.good());
    return {std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
}

std::string type_body(const std::string& schema, const std::string& name) {
    const auto start = schema.find("<xs:complexType name=\"" + name + "\"");
    REQUIRE(start != std::string::npos);
    const auto end = schema.find("</xs:complexType>", start);
    REQUIRE(end != std::string::npos);
    return schema.substr(start, end - start);
}

// Each collection type holds one entry element, written inline, whose
// attributes are its keys; the attribute id belongs to the collection itself.
std::map<std::string, kind_facts> schema_kinds() {
    const auto schema = read_schema();
    const auto root = type_body(schema, "todaysmarket");
    const std::regex child(R"re(<xs:element type="([A-Za-z]+)"\s+name=\s*"([A-Za-z]+)")re");
    const std::regex entry(R"re(<xs:element name="([A-Za-z]+)")re");
    const std::regex attribute(R"re(<xs:attribute type="[^"]+" name="([A-Za-z]+)")re");

    std::map<std::string, kind_facts> out;
    for (std::sregex_iterator it(root.begin(), root.end(), child), end; it != end; ++it) {
        const auto kind = (*it)[2].str();
        if (kind == "Configuration")
            continue;
        const auto collection = type_body(schema, (*it)[1].str());
        std::smatch m;
        REQUIRE(std::regex_search(collection, m, entry));
        std::vector<std::string> keys;
        for (std::sregex_iterator a(collection.begin(), collection.end(), attribute), e; a != e;
             ++a)
            if ((*a)[1].str() != "id")
                keys.push_back((*a)[1].str());
        REQUIRE(!keys.empty());
        out.emplace(kind, kind_facts{m[1].str(), keys[0], keys.size() > 1 ? keys[1] : ""});
    }
    return out;
}

}

TEST_CASE("the seeded collection kinds are the schema's, with their entries and keys", tags) {
    ores::testing::scoped_database_helper h;
    const auto expected = schema_kinds();
    INFO("Kinds are read from the schema by text patterns that expect a collection's "
         "type attribute before its name; a schema that changes that layout fails here.");
    REQUIRE(expected.size() == 24);

    const auto ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
    std::map<std::string, kind_facts> seeded;
    for (const auto& k :
         ores::analytics::repository::todays_market_collection_kind_repository().read_latest(ctx))
        seeded.emplace(k.code,
                       kind_facts{k.entry_element, k.key_attribute, k.key_attribute_2.value_or("")});
    CHECK(seeded == expected);
}
