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
#include "ores.storage/net/storage_paths.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>

using ores::storage::net::storage_paths;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.storage.tests");
const std::string tags("[net]");

}

TEST_CASE("make_object_path_uses_the_storage_api_prefix", tags) {
    auto lg(make_logger(test_suite));

    const auto path = storage_paths::make_object_path("compute-packages", "abc/def");

    BOOST_LOG_SEV(lg, info) << "Object path: " << path;
    CHECK(path == "/api/v1/storage/compute-packages/abc/def");
}

TEST_CASE("make_object_url_prepends_the_base_url", tags) {
    auto lg(make_logger(test_suite));

    const auto url = storage_paths::make_object_url("http://localhost:51000", "b", "k");

    BOOST_LOG_SEV(lg, info) << "Object URL: " << url;
    CHECK(url == "http://localhost:51000/api/v1/storage/b/k");
}

TEST_CASE("prefix_matches_the_storage_api_path", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Prefix: " << storage_paths::prefix;
    CHECK(storage_paths::prefix == "/api/v1/storage");
}

TEST_CASE("key_with_slashes_survives_unchanged", tags) {
    auto lg(make_logger(test_suite));

    const std::string key("releases/1.2.3/oscar-1.2.3.tar.gz");
    const auto path = storage_paths::make_object_path("compute-packages", key);
    const auto url =
        storage_paths::make_object_url("http://localhost:51000", "compute-packages", key);

    BOOST_LOG_SEV(lg, info) << "Hierarchical key path: " << path;
    CHECK(path == "/api/v1/storage/compute-packages/releases/1.2.3/oscar-1.2.3.tar.gz");
    CHECK(url == "http://localhost:51000/api/v1/storage/compute-packages/"
                 "releases/1.2.3/oscar-1.2.3.tar.gz");
}
