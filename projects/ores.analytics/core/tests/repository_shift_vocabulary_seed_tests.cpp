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
#include "ores.analytics.core/repository/shift_type_repository.hpp"
#include "ores.analytics.core/repository/stress_shift_family_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <catch2/catch_test_macros.hpp>
#include <fstream>
#include <iterator>
#include <regex>
#include <set>
#include <string>

/**
 * @file repository_shift_vocabulary_seed_tests.cpp
 * @brief The seeded shift families and shift types against the ORE schemas.
 *
 * Both vocabularies are read out of the schemas ORE ships, so a seed that
 * drops, adds or misspells a value fails here rather than when a document is
 * imported.
 */

namespace {

const std::string tags("[repository][shift_vocabulary][database]");

std::string read_schema(const std::string& name) {
    std::ifstream in(ores::testing::project_root::resolve("external/ore/xsd/" + name));
    REQUIRE(in.good());
    return {std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
}

std::string type_body(const std::string& schema, const std::string& kind, const std::string& name) {
    const auto start = schema.find("<xs:" + kind + " name=\"" + name + "\"");
    REQUIRE(start != std::string::npos);
    const auto end = schema.find("</xs:" + kind + ">", start);
    REQUIRE(end != std::string::npos);
    return schema.substr(start, end - start);
}

std::set<std::string> matches(const std::string& text, const std::regex& pattern) {
    std::set<std::string> out;
    for (std::sregex_iterator it(text.begin(), text.end(), pattern), end; it != end; ++it)
        out.insert((*it)[1].str());
    return out;
}

ores::database::context system_context(ores::testing::scoped_database_helper& h) {
    return h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
}

}

TEST_CASE("the seeded stress shift families are the stresstest type's elements", tags) {
    ores::testing::scoped_database_helper h;
    const auto body = type_body(read_schema("stress.xsd"), "complexType", "stresstest");
    auto expected = matches(body, std::regex(R"re(<xs:element[^>]*name="([A-Za-z]+)")re"));
    expected.erase("Date");
    REQUIRE(expected.size() == 16);

    std::set<std::string> seeded;
    for (const auto& f : ores::analytics::repository::stress_shift_family_repository().read_latest(
             system_context(h)))
        seeded.insert(f.code);
    CHECK(seeded == expected);
}

TEST_CASE("the seeded shift types are the schema's shiftType enumeration", tags) {
    ores::testing::scoped_database_helper h;
    const auto body = type_body(read_schema("ore_types.xsd"), "simpleType", "shiftType");
    const auto expected = matches(body, std::regex(R"re(<xs:enumeration value="([^"]+)")re"));
    REQUIRE(expected.size() == 3);

    std::set<std::string> seeded;
    for (const auto& t :
         ores::analytics::repository::shift_type_repository().read_latest(system_context(h)))
        seeded.insert(t.code);
    CHECK(seeded == expected);
}
