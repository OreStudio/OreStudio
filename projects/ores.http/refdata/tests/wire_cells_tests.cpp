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
#include "ores.http/routes/refdata/wire_cells.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/convention.hpp"
#include "ores.refdata.api/domain/convention_codec.hpp"
#include "ores.refdata.api/domain/curve_definition_codec.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <stdexcept>
#include <string>

using ores::http::routes::refdata::document::cell;
using ores::http::routes::refdata::document::find_cell;
using ores::http::routes::refdata::document::row_of;
using ores::http::routes::refdata::document::set_cell;
using ores::http::routes::refdata::document::text_of;
using namespace ores::logging;

namespace {

namespace refdata_domain = ores::refdata::domain;

const auto party_id = boost::uuids::string_generator()("00000000-0000-0000-0000-00000000000a");
const auto other_party_id =
    boost::uuids::string_generator()("00000000-0000-0000-0000-00000000000b");

/// One central convention, whose columns the wire groups onto three records.
refdata_domain::convention grouped_convention() {
    refdata_domain::convention row;
    row.identity.id = party_id;
    row.identity.party_id = party_id;
    row.identity.kind_code = "DEPOSIT";
    return row;
}

}

TEST_CASE("a_grouped_row_states_its_columns_by_their_own_names", "[wire_cells]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the read of a column a row groups";

    const auto row = grouped_convention();
    CHECK(find_cell(row, "party_id") == cell("00000000-0000-0000-0000-00000000000a"));
    CHECK(find_cell(row, "kind_code") == cell("DEPOSIT"));
    CHECK_FALSE(find_cell(row, "no_such_column").has_value());
}

TEST_CASE("a_grouped_row_writes_a_column_through_the_group_that_holds_it", "[wire_cells]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the write of a column a row groups";

    auto row = grouped_convention();
    CHECK(set_cell(row, "party_id", cell("00000000-0000-0000-0000-00000000000b")));
    CHECK(row.identity.party_id == other_party_id);
    CHECK(set_cell(row, "kind_code", cell("SWAP")));
    CHECK(row.identity.kind_code == "SWAP");
    CHECK_FALSE(set_cell(row, "no_such_column", cell("x")));
}

TEST_CASE("a_row_refuses_a_text_its_column_does_not_take", "[wire_cells]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the refusal of a text a column does not take";

    auto stated = refdata_domain::curve_definition{};
    CHECK(set_cell(stated, "position", cell("4")));
    CHECK(stated.position == 4);
    CHECK_THROWS_AS(set_cell(stated, "position", cell("many")), std::invalid_argument);
    CHECK_THROWS_AS(set_cell(stated, "id", cell("not-an-identifier")), std::invalid_argument);
}

TEST_CASE("a_flat_row_reads_as_the_cells_its_columns_hold", "[wire_cells]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the flattening of a row with no groups";

    refdata_domain::curve_definition stated;
    stated.id = party_id;
    stated.curve_id = "EUR-ESTR";
    stated.description = "ESTR discounting";
    stated.position = 4;

    const auto cells = row_of(stated);
    CHECK(text_of(cells, "curve_id") == "EUR-ESTR");
    CHECK(text_of(cells, "description") == "ESTR discounting");
    CHECK(text_of(cells, "position") == "4");
    CHECK(text_of(cells, "id") == "00000000-0000-0000-0000-00000000000a");
    CHECK(text_of(cells, "no_such_column").empty());
}
