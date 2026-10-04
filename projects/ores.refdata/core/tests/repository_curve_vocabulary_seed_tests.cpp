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
#include "ores.database/domain/context.hpp"
#include "ores.refdata.core/repository/calendar_name_repository.hpp"
#include "ores.refdata.core/repository/curve_section_repository.hpp"
#include "ores.refdata.core/repository/curve_segment_type_repository.hpp"
#include "ores.refdata.core/repository/day_counter_repository.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <catch2/catch_test_macros.hpp>
#include <fstream>
#include <iterator>
#include <map>
#include <regex>
#include <set>
#include <string>
#include <vector>

/**
 * @file repository_curve_vocabulary_seed_tests.cpp
 * @brief The seeded curve configuration vocabulary against the ORE schemas.
 *
 * The curve sections, the segment types and the day counter spellings are read
 * out of the schemas ORE ships, so a seed that drops, adds or misspells a value
 * fails here rather than when a document is imported.
 */

namespace {

const std::string tags("[repository][curve_vocabulary][database]");

std::string read_schema(const std::string& name) {
    const auto path = ores::testing::project_root::resolve("external/ore/xsd/" + name);
    std::ifstream in(path);
    REQUIRE(in.good());
    return {std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
}

// The text of one named top-level type, from its opening tag to its closing
// tag, so a value is only counted when it belongs to that type.
std::string type_body(const std::string& schema, const std::string& kind, const std::string& name) {
    const auto open = "<xs:" + kind + " name=\"" + name + "\"";
    const auto start = schema.find(open);
    REQUIRE(start != std::string::npos);
    const auto end = schema.find("</xs:" + kind + ">", start);
    REQUIRE(end != std::string::npos);
    return schema.substr(start, end - start);
}

std::vector<std::string> matches(const std::string& text, const std::regex& pattern) {
    std::vector<std::string> out;
    for (std::sregex_iterator it(text.begin(), text.end(), pattern), end; it != end; ++it)
        out.push_back((*it)[1].str());
    return out;
}

ores::database::context system_context(ores::testing::scoped_database_helper& h) {
    return h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
}

}

TEST_CASE("the seeded day counters are the schema's dayCounter enumeration", tags) {
    ores::testing::scoped_database_helper h;
    const auto body = type_body(read_schema("ore_types.xsd"), "simpleType", "dayCounter");
    const auto values = matches(body, std::regex(R"re(<xs:enumeration value="([^"]+)")re"));
    const std::set<std::string> expected(values.begin(), values.end());
    INFO("Values are read from the schema by text patterns that expect an element's "
         "type attribute before its name; a schema that changes that layout fails here.");
    REQUIRE(expected.size() == 71);

    ores::refdata::repository::day_counter_repository repo;
    std::set<std::string> seeded;
    for (const auto& d : repo.read_latest(system_context(h)))
        seeded.insert(d.code);
    CHECK(seeded == expected);
}

TEST_CASE("the seeded curve sections are the curve configuration's entry sections", tags) {
    ores::testing::scoped_database_helper h;
    const auto schema = read_schema("curveconfig.xsd");
    const auto root = type_body(schema, "complexType", "curveconfiguration");
    const std::regex element(R"re(<xs:element[^>]*name="([A-Za-z]+)")re");

    std::set<std::string> expected;
    for (const auto& name : matches(root, element)) {
        if (name != "ReportConfiguration")
            expected.insert(name);
    }
    INFO("Values are read from the schema by text patterns that expect an element's "
         "type attribute before its name; a schema that changes that layout fails here.");
    REQUIRE(expected.size() == 19);

    ores::refdata::repository::curve_section_repository repo;
    std::map<std::string, std::string> seeded;
    for (const auto& s : repo.read_latest(system_context(h)))
        seeded.emplace(s.code, s.entry_element);

    std::set<std::string> seeded_codes;
    for (const auto& [code, entry] : seeded)
        seeded_codes.insert(code);
    CHECK(seeded_codes == expected);
    CHECK(seeded.at("YieldCurves") == "YieldCurve");
    CHECK(seeded.at("FXVolatilities") == "FXVolatility");
    CHECK(seeded.at("InflationCapFloorVolatilities") == "InflationCapFloorVolatility");
}

TEST_CASE("the seeded segment types are the schema's segment types, each under its kind", tags) {
    ores::testing::scoped_database_helper h;
    const auto schema = read_schema("curveconfig.xsd");
    const auto segments = type_body(schema, "complexType", "segmentsType");
    const std::regex segment(R"re(<xs:element type="([A-Za-z]+)" name="([A-Za-z]+)")re");
    const std::regex fixed(R"re(name="Type" fixed="([^"]+)")re");
    const std::regex enumerated(R"re(type="([A-Za-z]+)" name="Type")re");
    const std::regex value(R"re(<xs:enumeration value="([^"]+)")re");

    std::map<std::string, std::string> expected;
    for (std::sregex_iterator it(segments.begin(), segments.end(), segment), end; it != end; ++it) {
        const auto kind = (*it)[2].str();
        const auto body = type_body(schema, "complexType", (*it)[1].str());
        if (const auto f = matches(body, fixed); !f.empty()) {
            expected.emplace(f.front(), kind);
            continue;
        }
        const auto e = matches(body, enumerated);
        REQUIRE(e.size() == 1);
        for (const auto& v : matches(type_body(schema, "simpleType", e.front()), value))
            expected.emplace(v, kind);
    }
    INFO("Values are read from the schema by text patterns that expect an element's "
         "type attribute before its name; a schema that changes that layout fails here.");
    REQUIRE(expected.size() == 21);

    ores::refdata::repository::curve_segment_type_repository repo;
    std::map<std::string, std::string> seeded;
    for (const auto& t : repo.read_latest(system_context(h)))
        seeded.emplace(t.code, t.segment_kind);
    CHECK(seeded == expected);
}

TEST_CASE("the seeded calendar names are the schema's calendar pattern names", tags) {
    ores::testing::scoped_database_helper h;
    const auto body = type_body(read_schema("ore_types.xsd"), "simpleType", "calendar");
    const std::string open_mark = "(^)?(";
    const std::string close_mark = "))*(\\))?";
    const auto start = body.find(open_mark);
    REQUIRE(start != std::string::npos);
    const auto end = body.find(close_mark, start);
    REQUIRE(end != std::string::npos);
    const auto alternatives = body.substr(start + open_mark.size(), end - start - open_mark.size());

    std::set<std::string> expected;
    std::size_t from = 0;
    while (from <= alternatives.size()) {
        const auto bar = alternatives.find('|', from);
        const auto name =
            alternatives.substr(from, bar == std::string::npos ? std::string::npos : bar - from);
        if (name != "[A-Z]{4}" && name != "CUSTOM_.*")
            expected.insert(name);
        if (bar == std::string::npos)
            break;
        from = bar + 1;
    }
    INFO("The calendar pattern's names are read between (^)?( and ))*(\\))?; the four-letter "
         "exchange code and CUSTOM_ forms are checked by form, not seeded.");
    REQUIRE(expected.size() == 471);

    ores::refdata::repository::calendar_name_repository repo;
    std::set<std::string> seeded;
    for (const auto& c : repo.read_latest(system_context(h)))
        seeded.insert(c.code);
    CHECK(seeded == expected);
}
