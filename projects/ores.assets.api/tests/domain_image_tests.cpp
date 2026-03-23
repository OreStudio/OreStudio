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
#include "ores.assets.api/domain/image.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.assets.api/domain/image_json_io.hpp" // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.assets.tests");
const std::string tags("[domain][image]");

}

using ores::assets::domain::image;
using namespace ores::logging;

TEST_CASE("create_image_with_key_and_description", tags) {
    auto lg(make_logger(test_suite));

    image sut;
    sut.key = "gb";
    sut.description = "Flag for country code GB (United Kingdom)";
    sut.modified_by = "admin";
    BOOST_LOG_SEV(lg, info) << "Image key: " << sut.key;

    CHECK(sut.key == "gb");
    CHECK(sut.description == "Flag for country code GB (United Kingdom)");
    CHECK(sut.modified_by == "admin");
}

TEST_CASE("default_constructed_image_has_zero_version", tags) {
    auto lg(make_logger(test_suite));

    const image sut;
    BOOST_LOG_SEV(lg, info) << "Default image version: " << sut.version;

    CHECK(sut.version == 0);
    CHECK(sut.key.empty());
    CHECK(sut.description.empty());
    CHECK(sut.svg_data.empty());
}

TEST_CASE("image_version_can_be_set", tags) {
    auto lg(make_logger(test_suite));

    image sut;
    sut.version = 3;
    sut.key = "ro";
    sut.description = "Flag for country code RO (Romania)";
    sut.modified_by = "admin";
    BOOST_LOG_SEV(lg, info) << "Image version: " << sut.version;

    CHECK(sut.version == 3);
}

TEST_CASE("image_json_output_contains_key_and_description", tags) {
    auto lg(make_logger(test_suite));

    image sut;
    sut.version = 1;
    sut.key = "us";
    sut.description = "Flag for country code US (United States)";
    sut.modified_by = "admin";
    sut.recorded_at = std::chrono::system_clock::now();

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "JSON output: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("us") != std::string::npos);
    CHECK(json_output.find("United States") != std::string::npos);
}
