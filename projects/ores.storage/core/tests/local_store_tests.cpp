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
#include "ores.platform/filesystem/scoped_temp_directory.hpp"
#include "ores.storage.core/filesystem/local_store.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>

using ores::platform::filesystem::scoped_temp_directory;
using ores::storage::filesystem::local_store;
using ores::storage::filesystem::object_not_found;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.storage.tests");
const std::string tags("[filesystem]");

}

TEST_CASE("a local store keeps the bytes it is given", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_directory root;
    local_store store(root.path());

    const std::string payload("the exact bytes, byte for byte");
    store.put("ores", "ore/imports/instance/trades.msgpack", payload);

    BOOST_LOG_SEV(lg, debug) << "Stored " << payload.size() << " bytes";

    CHECK(store.get("ores", "ore/imports/instance/trades.msgpack") == payload);
    CHECK(store.exists("ores", "ore/imports/instance/trades.msgpack"));
    CHECK(store.size("ores", "ore/imports/instance/trades.msgpack") == payload.size());
}

TEST_CASE("a local store reports an absent object distinctly", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_directory root;
    local_store store(root.path());

    CHECK_THROWS_AS(store.get("ores", "ore/imports/nothing/here"), object_not_found);
    CHECK_FALSE(store.exists("ores", "ore/imports/nothing/here"));
    CHECK_FALSE(store.size("ores", "ore/imports/nothing/here").has_value());
    CHECK_FALSE(store.remove("ores", "ore/imports/nothing/here"));

    BOOST_LOG_SEV(lg, debug) << "Absent object reported as absent";
}

TEST_CASE("a local store refuses a key that would leave its bucket", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_directory root;
    local_store store(root.path());
    store.put("ores", "compute/packages/app.tar.gz", "payload");

    CHECK_FALSE(store.is_valid_key("ores", "../ore/imports/secret"));
    CHECK_FALSE(store.exists("ores", "../ore/imports/secret"));
    CHECK_THROWS_AS(store.get("ores", "../ore/imports/secret"), object_not_found);

    CHECK_FALSE(local_store::is_valid_bucket("ores/../ore"));
    CHECK_FALSE(local_store::is_valid_bucket(".."));
    CHECK_FALSE(local_store::is_valid_bucket(""));
    CHECK(local_store::is_valid_bucket("ores"));

    BOOST_LOG_SEV(lg, debug) << "Traversal refused";
}

TEST_CASE("a local store lists a bucket by prefix", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_directory root;
    local_store store(root.path());

    store.put("ores", "compute/packages/a.tar.gz", "aa");
    store.put("ores", "compute/packages/b.tar.gz", "bbb");
    store.put("ores", "compute/input/a.tar.gz", "a");
    store.put("ores", "compute/output/a.tar.gz", "aaaa");

    const auto all = store.list("ores", "");
    REQUIRE(all.size() == 4);
    CHECK(all[0].key == "compute/input/a.tar.gz");
    CHECK(all[1].key == "compute/output/a.tar.gz");
    CHECK(all[2].key == "compute/packages/a.tar.gz");
    CHECK(all[3].key == "compute/packages/b.tar.gz");
    CHECK(all[2].size_bytes == 2);
    CHECK(all[3].size_bytes == 3);

    const auto filtered = store.list("ores", "compute/packages/");
    REQUIRE(filtered.size() == 2);
    CHECK(filtered[0].key == "compute/packages/a.tar.gz");
    CHECK(filtered[1].key == "compute/packages/b.tar.gz");

    CHECK(store.list("never-written", "").empty());

    BOOST_LOG_SEV(lg, debug) << "Listed " << all.size() << " objects";
}
