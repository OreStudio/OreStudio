/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.dq/domain/methodology.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/methodology_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/methodology_table.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

}

using ores::dq::domain::methodology;
using namespace ores::logging;

TEST_CASE("create_methodology_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    methodology sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.name = "Moving Average Calculation";
    sut.description = "Calculates simple moving average over a time window";
    sut.logic_reference = "https://docs.example.com/moving-average";
    sut.implementation_details = "Uses 30-day rolling window";
    sut.modified_by = "admin";
    sut.change_commentary = "Initial creation";
    BOOST_LOG_SEV(lg, info) << "Methodology: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.name == "Moving Average Calculation");
    CHECK(sut.description == "Calculates simple moving average over a time window");
    CHECK(sut.logic_reference == "https://docs.example.com/moving-average");
    CHECK(sut.implementation_details == "Uses 30-day rolling window");
    CHECK(sut.modified_by == "admin");
    CHECK(sut.change_commentary == "Initial creation");
}

TEST_CASE("methodology_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    methodology m;
    m.version = 1;
    m.id = boost::uuids::random_generator()();
    m.name = "Data Validation";
    m.description = "Validates data against schema rules";
    m.modified_by = "system";

    std::vector<methodology> methodologies = {m};
    auto table = convert_to_table(methodologies);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.contains("Name"));
    CHECK(table.contains("Description"));
    CHECK(table.contains("Data Validation"));
    CHECK(table.contains("Validates data against schema rules"));
}
