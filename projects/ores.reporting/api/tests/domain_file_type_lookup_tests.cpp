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
#include "ores.reporting.api/domain/file_type_lookup.hpp"
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <vector>

/**
 * @file domain_file_type_lookup_tests.cpp
 * @brief The file type rows a document's name is read from.
 *
 * The export of a configuration type states the file type that is the
 * configuration's whole ORE document, and the name it writes the file under
 * comes from that row rather than from code (RS-ORE-01). The cases below pin
 * which row answers when a type names more than one.
 */

namespace {

using namespace ores::logging;
using ores::reporting::domain::file_type;
using ores::reporting::domain::input_document_file_type_of;

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[domain][filetype]");

file_type make_file_type(const std::string& code,
                         const std::optional<std::string>& configuration_type_code,
                         const std::string& kind,
                         const std::string& direction,
                         const std::string& name_pattern,
                         bool is_default = false) {
    file_type type;
    type.version = 1;
    type.code = code;
    type.configuration_type_code = configuration_type_code;
    type.kind = kind;
    type.direction = direction;
    type.name_pattern = name_pattern;
    type.is_default = is_default;
    return type;
}

}

TEST_CASE("the_input_document_of_a_configuration_type_names_its_file", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<file_type> types{
        make_file_type("setup.pricingEnginesFile",
                       std::string("pricing_engines"),
                       "input_document",
                       "input",
                       "pricingengine.xml"),
        make_file_type("example.outputFileName", std::nullopt, "output_file", "output", "out.csv"),
    };

    const auto* type = input_document_file_type_of(types, "pricing_engines");
    REQUIRE(type != nullptr);
    CHECK(type->name_pattern == "pricingengine.xml");
    BOOST_LOG_SEV(lg, debug) << "Named: " << type->name_pattern;
}

TEST_CASE("a_row_for_another_kind_or_direction_does_not_answer", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<file_type> types{
        make_file_type("a.data", std::string("pricing_engines"), "data_file", "input", "x.csv"),
        make_file_type(
            "b.output", std::string("pricing_engines"), "input_document", "output", "y.xml"),
        make_file_type("c.other", std::string("todays_market"), "input_document", "input", "z.xml"),
    };

    CHECK(input_document_file_type_of(types, "pricing_engines") == nullptr);
}

TEST_CASE("the_default_row_answers_when_a_type_names_several", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<file_type> types{
        make_file_type(
            "a.first", std::string("pricing_engines"), "input_document", "input", "first.xml"),
        make_file_type("b.default",
                       std::string("pricing_engines"),
                       "input_document",
                       "input",
                       "default.xml",
                       true),
    };

    const auto* type = input_document_file_type_of(types, "pricing_engines");
    REQUIRE(type != nullptr);
    CHECK(type->name_pattern == "default.xml");
}

TEST_CASE("no_row_states_no_name", tags) {
    auto lg(make_logger(test_suite));

    CHECK(input_document_file_type_of({}, "pricing_engines") == nullptr);
    BOOST_LOG_SEV(lg, debug) << "A type with no row is refused by the export.";
}
