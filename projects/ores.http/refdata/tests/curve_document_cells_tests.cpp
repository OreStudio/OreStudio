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
#include "ores.http/routes/refdata/curve_document_cells.hpp"
#include "ores.http/routes/refdata/curve_section_wiring.hpp"
#include "ores.http/routes/refdata/wire_cells.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using ores::http::routes::refdata::document::cell_at;
using ores::http::routes::refdata::document::cells;
using ores::http::routes::refdata::document::number_of;
using ores::http::routes::refdata::document::text_of;
using namespace ores::logging;

namespace {

namespace refdata_domain = ores::refdata::domain;
namespace refdata_messaging = ores::refdata::messaging;

/// The document one entry of a yield curve needs, and nothing else.
refdata_messaging::curve_configuration_document document_with_one_curve() {
    refdata_messaging::curve_configuration_document document;
    document.config.name = "ACME curves";
    document.config.description = "The curves the desk trades";
    document.config.modified_by = "desk";
    document.config.version = 3;

    refdata_domain::curve_definition stated;
    stated.id = boost::uuids::string_generator()("00000000-0000-0000-0000-00000000000a");
    stated.curve_id = "EUR-ESTR";
    stated.description = "ESTR discounting";
    stated.section_code = "YieldCurves";
    stated.position = 4;
    stated.version = 2;
    document.definitions.push_back(stated);

    refdata_domain::curve_definition unstated = stated;
    unstated.id = boost::uuids::string_generator()("00000000-0000-0000-0000-00000000000b");
    unstated.curve_id = "EUR-3M";
    unstated.description = std::string();
    unstated.position = 5;
    document.definitions.push_back(unstated);

    refdata_domain::yield_curve_config curve;
    curve.curve_definition_id = stated.id;
    curve.currency = "EUR";
    curve.day_counter = "ACT/360";
    document.yield_curves.push_back(curve);
    return document;
}

}

TEST_CASE("a_curve_document_row_is_read_by_the_column_it_states", "[curve_document_cells]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the flattening of a curve configuration document";

    const auto document = cells(document_with_one_curve());

    CHECK(text_of(document.header(), "name") == "ACME curves");
    CHECK(text_of(document.header(), "description") == "The curves the desk trades");
    CHECK(number_of(document.header(), "version") == 3);
    CHECK(text_of(document.header(), "recorded_at") == "1970-01-01 00:00:00.000000Z");

    const auto* definitions = document.find("definitions");
    REQUIRE(definitions != nullptr);
    REQUIRE(definitions->size() == 2);
    CHECK(text_of(definitions->at(0), "curve_id") == "EUR-ESTR");
    CHECK(text_of(definitions->at(0), "id") == "00000000-0000-0000-0000-00000000000a");
    CHECK(number_of(definitions->at(0), "position") == 4);
    CHECK(number_of(definitions->at(0), "version") == 2);

    const auto* curves = document.find("yield_curves");
    REQUIRE(curves != nullptr);
    REQUIRE(curves->size() == 1);
    CHECK(text_of(curves->at(0), "currency") == "EUR");
    CHECK(text_of(curves->at(0), "curve_definition_id") == "00000000-0000-0000-0000-00000000000a");
}

TEST_CASE("a_curve_document_cell_is_absent_empty_or_stated", "[curve_document_cells]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a cell a curve document row does not carry";

    const auto document = cells(document_with_one_curve());
    const auto& definitions = document.at("definitions");
    REQUIRE(definitions.size() == 2);

    CHECK(cell_at(definitions.at(0), "description") ==
          std::optional<std::string>("ESTR discounting"));
    CHECK(cell_at(definitions.at(1), "description") == std::optional<std::string>(""));
    CHECK_FALSE(cell_at(definitions.at(0), "no_such_column").has_value());
    CHECK(text_of(definitions.at(0), "no_such_column").empty());
    CHECK(number_of(definitions.at(0), "no_such_column") == 0);
    CHECK(document.at("no_such_vector").empty());
}

TEST_CASE("every_vector_the_curve_screen_reads_is_one_a_document_carries",
          "[curve_document_cells]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the vectors the curve screen's wiring names";

    namespace wiring = ores::http::routes::refdata::wiring;
    const auto document = cells(refdata_messaging::curve_configuration_document{});
    std::vector<std::string> read{"definitions", "quotes"};
    for (const auto& section : wiring::sections()) {
        for (const auto& entity : section.entities)
            read.emplace_back(entity.vector);
        for (const auto& collection : section.collections) {
            read.emplace_back(collection.vector);
            for (const auto& kind : collection.kinds)
                for (const auto& leaf : kind.leaves)
                    read.emplace_back(leaf.vector);
        }
    }

    for (const auto& vector : read)
        CHECK(document.find(vector) != nullptr);
}
