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
#include "ores.logging/logger.hpp"
#include "ores.platform/filesystem/scoped_temp_directory.hpp"
#include "ores.storage.core/filesystem/local_store.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <string>
#include <string_view>

using ores::platform::filesystem::scoped_temp_directory;
using ores::storage::filesystem::local_store;
using ores::storage::filesystem::object_not_found;
using namespace ores::logging;

TEST_CASE("a local store keeps the bytes it is given", "[filesystem]") {
    auto lg(make_logger(test_suite));

    scoped_temp_directory root;
    local_store store(root.path());

    const std::string payload("the exact bytes, byte for byte");
    store.put("ores", "ore/imports/instance/trades.msgpack", payload);

    ORES_LOG_SEV(lg, debug) << "Stored " << payload.size() << " bytes";

    CHECK(store.get("ores", "ore/imports/instance/trades.msgpack") == payload);
    CHECK(store.exists("ores", "ore/imports/instance/trades.msgpack"));
    CHECK(store.size("ores", "ore/imports/instance/trades.msgpack") == payload.size());
}

TEST_CASE("a local store reports an absent object distinctly", "[filesystem]") {
    auto lg(make_logger(test_suite));

    scoped_temp_directory root;
    local_store store(root.path());

    CHECK_THROWS_AS(store.get("ores", "ore/imports/nothing/here"), object_not_found);
    CHECK_FALSE(store.exists("ores", "ore/imports/nothing/here"));
    CHECK_FALSE(store.size("ores", "ore/imports/nothing/here").has_value());
    CHECK_FALSE(store.remove("ores", "ore/imports/nothing/here"));

    ORES_LOG_SEV(lg, debug) << "Absent object reported as absent";
}

TEST_CASE("a local store refuses a key that would leave its bucket", "[filesystem]") {
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

    ORES_LOG_SEV(lg, debug) << "Traversal refused";
}

TEST_CASE("a local store lists a bucket by prefix", "[filesystem]") {
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

    ORES_LOG_SEV(lg, debug) << "Listed " << all.size() << " objects";
}

TEST_CASE("a listing states when each object was written", "[filesystem]") {
    auto lg(make_logger(test_suite));

    scoped_temp_directory root;
    local_store store(root.path());

    const auto before = std::chrono::system_clock::now();
    store.put("ores", "compute/input/recent.tar.gz", "a");
    store.put("ores", "compute/input/old.tar.gz", "a");
    const auto after = std::chrono::system_clock::now();

    const auto old = root.path() / "ores" / "compute" / "input" / "old.tar.gz";
    std::filesystem::last_write_time(old,
                                     std::filesystem::last_write_time(old) - std::chrono::days(10));

    const auto entries = store.list("ores", "compute/input/");
    REQUIRE(entries.size() == 2);
    REQUIRE(entries[0].key == "compute/input/old.tar.gz");
    REQUIRE(entries[1].key == "compute/input/recent.tar.gz");

    // The filesystem states a write to its own resolution, so the instant is
    // the write's to within a second rather than to the clock's.
    CHECK(entries[1].last_modified >= before - std::chrono::seconds(1));
    CHECK(entries[1].last_modified <= after + std::chrono::seconds(1));

    const auto age = std::chrono::system_clock::now() - entries[0].last_modified;
    CHECK(age > std::chrono::days(9));
    CHECK(age < std::chrono::days(11));

    ORES_LOG_SEV(lg, debug) << "Listed age of " << entries[0].key;
}

TEST_CASE("an expiry reports the objects past its cutoff and removes none", "[filesystem]") {
    auto lg(make_logger(test_suite));

    scoped_temp_directory root;
    local_store store(root.path());

    store.put("ores", "compute/input/old.tar.gz", "aa");
    store.put("ores", "compute/input/recent.tar.gz", "bbb");
    const auto old = root.path() / "ores" / "compute" / "input" / "old.tar.gz";
    std::filesystem::last_write_time(old,
                                     std::filesystem::last_write_time(old) - std::chrono::days(10));

    const auto state = store.expire("ores",
                                    "compute/input/",
                                    std::chrono::system_clock::now() - std::chrono::days(7),
                                    100,
                                    true);

    CHECK(state.selected == 1);
    CHECK(state.selected_bytes == 2);
    CHECK(state.removed == 0);
    CHECK(state.remaining == 0);
    CHECK(store.list("ores", "compute/input/").size() == 2);

    ORES_LOG_SEV(lg, debug) << "Dry run selected " << state.selected;
}

TEST_CASE("an expiry removes the objects past its cutoff and keeps the rest", "[filesystem]") {
    auto lg(make_logger(test_suite));

    scoped_temp_directory root;
    local_store store(root.path());

    store.put("ores", "compute/input/old.tar.gz", "aa");
    store.put("ores", "compute/input/recent.tar.gz", "bbb");
    const auto old = root.path() / "ores" / "compute" / "input" / "old.tar.gz";
    std::filesystem::last_write_time(old,
                                     std::filesystem::last_write_time(old) - std::chrono::days(10));

    const auto state = store.expire("ores",
                                    "compute/input/",
                                    std::chrono::system_clock::now() - std::chrono::days(7),
                                    100,
                                    false);

    CHECK(state.selected == 1);
    CHECK(state.removed == 1);
    CHECK(state.removed_bytes == 2);

    const auto left = store.list("ores", "compute/input/");
    REQUIRE(left.size() == 1);
    CHECK(left[0].key == "compute/input/recent.tar.gz");

    ORES_LOG_SEV(lg, debug) << "Removed " << state.removed;
}

TEST_CASE("an expiry past its limit states what it left for the next call", "[filesystem]") {
    auto lg(make_logger(test_suite));

    scoped_temp_directory root;
    local_store store(root.path());

    store.put("ores", "compute/input/a.tar.gz", "a");
    store.put("ores", "compute/input/b.tar.gz", "b");
    store.put("ores", "compute/input/c.tar.gz", "c");
    for (const auto* key : {"a.tar.gz", "b.tar.gz", "c.tar.gz"}) {
        const auto path = root.path() / "ores" / "compute" / "input" / key;
        std::filesystem::last_write_time(
            path, std::filesystem::last_write_time(path) - std::chrono::days(10));
    }

    const auto cutoff = std::chrono::system_clock::now() - std::chrono::days(7);
    const auto first = store.expire("ores", "compute/input/", cutoff, 2, false);

    CHECK(first.selected == 2);
    CHECK(first.removed == 2);
    CHECK(first.remaining == 1);
    CHECK(store.list("ores", "compute/input/").size() == 1);

    const auto second = store.expire("ores", "compute/input/", cutoff, 2, false);

    CHECK(second.selected == 1);
    CHECK(second.removed == 1);
    CHECK(second.remaining == 0);
    CHECK(store.list("ores", "compute/input/").empty());

    ORES_LOG_SEV(lg, debug) << "Two calls removed " << first.removed + second.removed;
}

TEST_CASE("an expiry leaves an object at the cutoff instant", "[filesystem]") {
    auto lg(make_logger(test_suite));

    scoped_temp_directory root;
    local_store store(root.path());

    store.put("ores", "compute/input/edge.tar.gz", "a");
    const auto listed = store.list("ores", "compute/input/");
    REQUIRE(listed.size() == 1);

    const auto listed_at = listed[0].last_modified;
    const auto after =
        store.expire("ores", "compute/input/", listed_at + std::chrono::seconds(1), 100, true);
    const auto before =
        store.expire("ores", "compute/input/", listed_at - std::chrono::seconds(1), 100, true);

    CHECK(after.selected == 1);
    CHECK(before.selected == 0);

    ORES_LOG_SEV(lg, debug) << "The cutoff is the boundary the object is on";
}

TEST_CASE("a part file is named for one caller and published by its own hand", "[filesystem]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the part-file protocol the streamed upload uses";

    scoped_temp_directory root;
    local_store store(root.path());

    const auto first = store.begin_put("bucket", "key");
    const auto second = store.begin_put("bucket", "key");

    // Two callers writing one key never share a file.
    REQUIRE(first != second);
    REQUIRE(!store.exists("bucket", "key"));

    {
        std::ofstream out(first, std::ios::binary | std::ios::trunc);
        REQUIRE(static_cast<bool>(out));
        out << "first";
    }
    {
        std::ofstream out(second, std::ios::binary | std::ios::trunc);
        REQUIRE(static_cast<bool>(out));
        out << "second";
    }

    // A part file is not an object, so it is not listed as one.
    REQUIRE(store.list("bucket", "").empty());

    store.commit_put(first, "bucket", "key");
    REQUIRE(store.get("bucket", "key") == "first");

    store.commit_put(second, "bucket", "key");
    REQUIRE(store.get("bucket", "key") == "second");

    // Discarding a part that is already gone is not an error.
    REQUIRE_NOTHROW(local_store::abandon_put(first));
    REQUIRE_NOTHROW(local_store::abandon_put(root.path() / "nothing.here"));
}
