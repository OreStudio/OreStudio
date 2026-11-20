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
#include "ores.http/routes/refdata/curve_document_edit.hpp"
#include "ores.logging/logger.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <stdexcept>
#include <string>

using ores::http::routes::refdata::document::cell;
using ores::http::routes::refdata::document::cell_at;
using ores::http::routes::refdata::document::cells;
using ores::http::routes::refdata::document::copy_vector;
using ores::http::routes::refdata::document::text_of;
using ores::http::routes::refdata::document::write_column;
using namespace ores::logging;

namespace {

namespace refdata_domain = ores::refdata::domain;
namespace refdata_messaging = ores::refdata::messaging;

const auto edit_definition_id =
    boost::uuids::string_generator()("00000000-0000-0000-0000-00000000000a");
const auto other_edit_definition_id =
    boost::uuids::string_generator()("00000000-0000-0000-0000-00000000000b");

/// A document with two yield curves, whose rows the changes are written to.
refdata_messaging::curve_configuration_document two_curves() {
    refdata_messaging::curve_configuration_document document;
    document.config.name = "ACME curves";

    refdata_domain::curve_definition stated;
    stated.id = edit_definition_id;
    stated.curve_id = "EUR-ESTR";
    stated.description = "ESTR discounting";
    stated.section_code = "YieldCurves";
    stated.position = 4;
    document.definitions.push_back(stated);

    refdata_domain::curve_definition other = stated;
    other.id = other_edit_definition_id;
    other.curve_id = "EUR-3M";
    other.description.reset();
    document.definitions.push_back(other);

    refdata_domain::yield_curve_config curve;
    curve.curve_definition_id = edit_definition_id;
    curve.currency = "EUR";
    document.yield_curves.push_back(curve);
    return document;
}

}

TEST_CASE("a_curve_change_writes_one_column_of_the_row_it_names", "[curve_document_edit]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the write of one leaf of one curve entry";

    auto document = two_curves();
    CHECK(write_column(document,
                       "yield_curves",
                       "curve_definition_id",
                       "00000000-0000-0000-0000-00000000000a",
                       "currency",
                       cell("GBP")));
    CHECK(write_column(document,
                       "definitions",
                       "id",
                       "00000000-0000-0000-0000-00000000000b",
                       "description",
                       cell("3M IBOR")));
    CHECK(write_column(document,
                       "definitions",
                       "id",
                       "00000000-0000-0000-0000-00000000000a",
                       "position",
                       cell("9")));

    const auto read = cells(document);
    const auto& curves = read.at("yield_curves");
    REQUIRE(curves.size() == 1);
    CHECK(text_of(curves.at(0), "currency") == "GBP");
    const auto& definitions = read.at("definitions");
    REQUIRE(definitions.size() == 2);
    CHECK(text_of(definitions.at(1), "description") == "3M IBOR");
    CHECK(text_of(definitions.at(0), "position") == "9");
}

TEST_CASE("a_curve_change_unsets_a_column_it_states_as_nothing", "[curve_document_edit]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the unset of one curve leaf";

    auto document = two_curves();
    CHECK(write_column(document,
                       "definitions",
                       "id",
                       "00000000-0000-0000-0000-00000000000a",
                       "description",
                       cell()));

    const auto read = cells(document);
    CHECK_FALSE(cell_at(read.at("definitions").at(0), "description").has_value());
}

TEST_CASE("a_curve_change_answers_the_rows_it_cannot_write", "[curve_document_edit]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the rows and columns a change cannot write";

    auto document = two_curves();
    CHECK_FALSE(write_column(document,
                             "yield_curves",
                             "curve_definition_id",
                             "00000000-0000-0000-0000-0000000000ff",
                             "currency",
                             cell("GBP")));
    CHECK_FALSE(write_column(document,
                             "yield_curves",
                             "curve_definition_id",
                             "00000000-0000-0000-0000-00000000000a",
                             "no_such_column",
                             cell("GBP")));
    CHECK_FALSE(write_column(document,
                             "no_such_vector",
                             "id",
                             "00000000-0000-0000-0000-00000000000a",
                             "currency",
                             cell("GBP")));
    CHECK_THROWS_AS(write_column(document,
                                 "definitions",
                                 "id",
                                 "00000000-0000-0000-0000-00000000000a",
                                 "position",
                                 cell("many")),
                    std::invalid_argument);
}

TEST_CASE("a_curve_save_copies_only_the_tables_it_touched", "[curve_document_edit]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the tables a curve save writes back";

    const auto document = two_curves();
    refdata_messaging::curve_configuration_document saved;
    saved.config = document.config;
    CHECK(copy_vector(document, "yield_curves", saved));
    CHECK_FALSE(copy_vector(document, "no_such_vector", saved));

    CHECK(saved.config.name == "ACME curves");
    CHECK(saved.yield_curves.size() == 1);
    CHECK(saved.definitions.empty());
    CHECK(saved.segments.empty());
}
