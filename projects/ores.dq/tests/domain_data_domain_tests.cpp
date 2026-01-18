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
#include "ores.dq/domain/data_domain.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/data_domain_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/data_domain_table.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

}

using ores::dq::domain::data_domain;
using namespace ores::logging;

TEST_CASE("create_data_domain_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    data_domain sut;
    sut.version = 1;
    sut.name = "Reference Data";
    sut.description = "Static reference data like currencies and countries";
    sut.recorded_by = "admin";
    sut.change_commentary = "Initial creation";
    BOOST_LOG_SEV(lg, info) << "Data domain: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.name == "Reference Data");
    CHECK(sut.description == "Static reference data like currencies and countries");
    CHECK(sut.recorded_by == "admin");
    CHECK(sut.change_commentary == "Initial creation");
}

TEST_CASE("data_domain_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    data_domain dd;
    dd.version = 1;
    dd.name = "Market Data";
    dd.description = "Real-time and historical market prices";
    dd.recorded_by = "system";

    std::vector<data_domain> domains = {dd};
    auto table = convert_to_table(domains);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("Name") != std::string::npos);
    CHECK(table.find("Description") != std::string::npos);
    CHECK(table.find("Market Data") != std::string::npos);
    CHECK(table.find("Real-time and historical market prices") != std::string::npos);
}
