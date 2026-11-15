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
#include "ores.utility/codec/json_schema.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <vector>

namespace schema_example {

enum class side { buy, sell };

struct example_row {
    std::string name;
    std::optional<std::string> note;
    std::int64_t count = 0;
    double ratio = 0.0;
    bool live = false;
    side direction = side::buy;
    std::vector<std::string> codes = {};
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<example_row>) {
    using ::ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"name", &example_row::name},
                                              field{"note", &example_row::note},
                                              field{"count", &example_row::count},
                                              field{"ratio", &example_row::ratio},
                                              field{"live", &example_row::live},
                                              field{"direction", &example_row::direction},
                                              field{"codes", &example_row::codes}};
}

}

TEST_CASE("a schema states each member's type", "[codec][json][schema]") {
    const auto schema = ores::utility::codec::json_schema<schema_example::example_row>();

    CHECK(schema.find(R"("type":"object")") != std::string::npos);
    CHECK(schema.find(R"("name":{"type":"string"})") != std::string::npos);
    CHECK(schema.find(R"("count":{"type":"integer"})") != std::string::npos);
    CHECK(schema.find(R"("ratio":{"type":"number"})") != std::string::npos);
    CHECK(schema.find(R"("live":{"type":"boolean"})") != std::string::npos);
    CHECK(schema.find(R"("codes":{"type":"array","items":{"type":"string"}})") !=
          std::string::npos);
}

TEST_CASE("an enumeration names the text each value has", "[codec][json][schema]") {
    const auto schema = ores::utility::codec::json_schema<schema_example::example_row>();

    CHECK(schema.find(R"("direction":{"type":"string","enum":["buy","sell"]})") !=
          std::string::npos);
}

TEST_CASE("a member the shape leaves out is not required", "[codec][json][schema]") {
    const auto schema = ores::utility::codec::json_schema<schema_example::example_row>();

    CHECK(schema.find(R"("required":["name","count","ratio","live","direction","codes"])") !=
          std::string::npos);
}

TEST_CASE("an optional member may be null", "[codec][json][schema]") {
    const auto schema = ores::utility::codec::json_schema<schema_example::example_row>();

    CHECK(schema.find(R"("note":{"type":"string","nullable":true})") != std::string::npos);
}
