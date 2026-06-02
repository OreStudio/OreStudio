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
#include "ores.compute.api/domain/app_version_platform.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.compute.api/domain/app_version_platform_json_io.hpp" // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.compute.tests");
const std::string tags("[domain]");

}

using ores::compute::domain::app_version_platform;
using namespace ores::logging;

TEST_CASE("create_app_version_platform_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    app_version_platform sut;
    sut.app_version_id = boost::uuids::random_generator()();
    sut.platform_id = boost::uuids::random_generator()();
    sut.platform_code = "x64-linux";
    sut.package_uri =
        "/api/v1/storage/compute/packages/ore/1.8.15.0/ore-1.8.15.0-x64-linux.tar.gz";

    BOOST_LOG_SEV(lg, info) << "AppVersionPlatform: " << sut;

    CHECK(sut.platform_code == "x64-linux");
    CHECK(sut.package_uri.find("x64-linux.tar.gz") != std::string::npos);
    CHECK(!sut.app_version_id.is_nil());
    CHECK(!sut.platform_id.is_nil());
}

TEST_CASE("create_app_version_platform_with_specific_uuids", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto av_id = uuid_gen("550e8400-e29b-41d4-a716-446655440001");
    const auto pl_id = uuid_gen("550e8400-e29b-41d4-a716-446655440002");

    app_version_platform sut;
    sut.app_version_id = av_id;
    sut.platform_id = pl_id;
    sut.platform_code = "arm64-osx";
    sut.package_uri = "s3://example/ore-arm64-osx.tar.gz";

    BOOST_LOG_SEV(lg, info) << "AppVersionPlatform: " << sut;

    CHECK(sut.app_version_id == av_id);
    CHECK(sut.platform_id == pl_id);
    CHECK(sut.platform_code == "arm64-osx");
}

TEST_CASE("app_version_platform_insertion_operator_emits_json", tags) {
    auto lg(make_logger(test_suite));

    app_version_platform sut;
    sut.app_version_id = boost::uuids::random_generator()();
    sut.platform_id = boost::uuids::random_generator()();
    sut.platform_code = "x64-windows";
    sut.package_uri = "s3://example/ore-x64-windows.tar.gz";

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "AppVersionPlatform JSON: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("x64-windows") != std::string::npos);
    CHECK(json_output.find("ore-x64-windows.tar.gz") != std::string::npos);
}

TEST_CASE("app_version_platform_default_construction_has_empty_strings", tags) {
    const app_version_platform sut;

    CHECK(sut.platform_code.empty());
    CHECK(sut.package_uri.empty());
    CHECK(sut.app_version_id.is_nil());
    CHECK(sut.platform_id.is_nil());
}
