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
#include "ores.reporting.api/domain/report_type.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/domain/report_type_table.hpp"
#include "ores.reporting.api/domain/report_type_table_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/generators/report_type_generator.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace {

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[domain]");

}

using ores::reporting::domain::report_type;
using ores::reporting::generators::generate_synthetic_report_type;
using ores::reporting::generators::generate_synthetic_report_types;
using ores::utility::generation::generation_context;
using namespace ores::logging;

TEST_CASE("create_report_type_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    report_type sut;
    sut.version = 1;
    sut.code = "risk";
    sut.name = "Risk Report";
    sut.description = "ORE risk calculation report";
    sut.display_order = 1;
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Initial creation";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Report type: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "risk");
    CHECK(sut.name == "Risk Report");
    CHECK(sut.display_order == 1);
    CHECK(sut.modified_by == "system");
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("report_type_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    report_type sut;
    sut.version = 1;
    sut.code = "grid";
    sut.name = "Grid Report";
    sut.description = "ORE grid sensitivity report";
    sut.display_order = 2;
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Test";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Report type: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("grid") != std::string::npos);
}

TEST_CASE("create_report_type_with_faker", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    auto sut = generate_synthetic_report_type(ctx);
    BOOST_LOG_SEV(lg, info) << "Report type: " << sut;

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("create_multiple_random_report_types", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    const std::size_t count = 3;
    auto items = generate_synthetic_report_types(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        BOOST_LOG_SEV(lg, info) << "Report type: " << item;
        CHECK(!item.code.empty());
        CHECK(item.version == 1);
    }
}

TEST_CASE("report_type_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    report_type rt;
    rt.version = 1;
    rt.code = "risk";
    rt.name = "Risk";
    rt.description = "Risk report";
    rt.display_order = 1;
    rt.modified_by = "system";
    rt.performed_by = "system";
    rt.change_reason_code = "system.new";
    rt.change_commentary = "Test";
    rt.recorded_at = std::chrono::system_clock::now();

    std::vector<report_type> items = {rt};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("risk") != std::string::npos);
}

TEST_CASE("report_type_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<report_type> items;
    for (int i = 0; i < 3; ++i) {
        report_type rt;
        rt.version = 1;
        rt.code = "type" + std::to_string(i);
        rt.name = "Type " + std::to_string(i);
        rt.description = "Description " + std::to_string(i);
        rt.display_order = i + 1;
        rt.modified_by = "system";
        rt.performed_by = "system";
        rt.change_reason_code = "system.new";
        rt.change_commentary = "Test";
        rt.recorded_at = std::chrono::system_clock::now();
        items.push_back(rt);
    }

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("type0") != std::string::npos);
    CHECK(table.find("type1") != std::string::npos);
    CHECK(table.find("type2") != std::string::npos);
}

TEST_CASE("report_type_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<report_type> items;
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("report_type_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    auto items = generate_synthetic_report_types(5, ctx);
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& item : items) {
        CHECK(table.find(item.code) != std::string::npos);
    }
}
