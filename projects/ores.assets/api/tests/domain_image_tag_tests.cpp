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
#include "ores.assets.api/domain/image_tag.hpp"
#include "ores.assets.api/domain/image_tag_json_io.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <sstream>

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string tags("[domain][image_tag]");

}

using ores::assets::domain::image_tag;
using namespace ores::logging;

TEST_CASE("default_constructed_image_tag_has_empty_fields", tags) {
    auto lg(make_logger(test_suite));

    const image_tag sut;
    BOOST_LOG_SEV(lg, info) << "Default image_tag assigned_by: " << sut.assigned_by;

    CHECK(sut.version == 0);
    CHECK(sut.tenant_id.empty());
    CHECK(sut.image_id.is_nil());
    CHECK(sut.tag_id.is_nil());
    CHECK(sut.assigned_by.empty());
    CHECK(sut.assigned_at == std::chrono::system_clock::time_point{});
}

TEST_CASE("image_tag_assigned_by_can_be_set", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator gen;
    const auto image_id = gen("00000000-0000-0000-0000-000000000001");
    const auto tag_id = gen("00000000-0000-0000-0000-000000000002");

    image_tag sut;
    sut.image_id = image_id;
    sut.tag_id = tag_id;
    sut.assigned_by = "admin";
    sut.assigned_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "image_tag assigned_by: " << sut.assigned_by;

    CHECK(sut.image_id == image_id);
    CHECK(sut.tag_id == tag_id);
    CHECK(sut.assigned_by == "admin");
}

TEST_CASE("image_tag_json_output_contains_keys_and_actor", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator gen;
    const auto image_id = gen("00000000-0000-0000-0000-000000000001");
    const auto tag_id = gen("00000000-0000-0000-0000-000000000002");

    image_tag sut;
    sut.image_id = image_id;
    sut.tag_id = tag_id;
    sut.assigned_by = "admin";
    sut.assigned_at = std::chrono::system_clock::now();

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("\"assigned_by\":\"admin\"") != std::string::npos);
    CHECK(json_output.find("00000000-0000-0000-0000-000000000001") != std::string::npos);
    CHECK(json_output.find("00000000-0000-0000-0000-000000000002") != std::string::npos);
}
