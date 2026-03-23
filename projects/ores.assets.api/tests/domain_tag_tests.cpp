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
#include "ores.assets.api/domain/tag.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.assets.api/domain/tag_json_io.hpp" // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string tags("[domain][tag]");

}

using ores::assets::domain::tag;
using namespace ores::logging;

TEST_CASE("create_tag_with_name_and_description", tags) {
    auto lg(make_logger(test_suite));

    tag sut;
    sut.name = "flag";
    sut.description = "National flags for country codes";
    sut.modified_by = "admin";
    BOOST_LOG_SEV(lg, info) << "Tag name: " << sut.name;

    CHECK(sut.name == "flag");
    CHECK(sut.description == "National flags for country codes");
    CHECK(sut.modified_by == "admin");
}

TEST_CASE("default_constructed_tag_has_zero_version", tags) {
    auto lg(make_logger(test_suite));

    const tag sut;
    BOOST_LOG_SEV(lg, info) << "Default tag version: " << sut.version;

    CHECK(sut.version == 0);
    CHECK(sut.name.empty());
    CHECK(sut.description.empty());
    CHECK(sut.tag_id.empty());
    CHECK(sut.tenant_id.empty());
}

TEST_CASE("tag_provenance_fields_can_be_set", tags) {
    auto lg(make_logger(test_suite));

    tag sut;
    sut.name = "currency";
    sut.description = "Currency symbol images";
    sut.modified_by = "admin";
    sut.change_reason_code = "INITIAL_LOAD";
    sut.change_commentary = "Initial data load";
    sut.performed_by = "system";
    BOOST_LOG_SEV(lg, info) << "Tag name: " << sut.name;

    CHECK(sut.change_reason_code == "INITIAL_LOAD");
    CHECK(sut.change_commentary == "Initial data load");
    CHECK(sut.performed_by == "system");
}

TEST_CASE("tag_json_output_contains_name", tags) {
    auto lg(make_logger(test_suite));

    tag sut;
    sut.version = 1;
    sut.name = "commodity";
    sut.description = "Commodity images";
    sut.modified_by = "admin";

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("commodity") != std::string::npos);
}
