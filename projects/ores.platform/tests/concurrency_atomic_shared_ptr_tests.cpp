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
#include "ores.platform/concurrency/atomic_shared_ptr.hpp"
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <string>
#include <thread>
#include <vector>

namespace {

const std::string_view test_suite("ores.platform.tests");
const std::string tags("[concurrency][atomic_shared_ptr]");

using ores::platform::concurrency::atomic_shared_ptr;

/// An immutable settings-shaped payload whose two halves must always agree.
struct snapshot {
    std::string name;
    int revision;
};

}

using namespace ores::logging;

TEST_CASE("a_default_constructed_snapshot_is_empty", tags) {
    auto lg(make_logger(test_suite));

    const atomic_shared_ptr<const snapshot> published;

    BOOST_LOG_SEV(lg, info) << "Reading a snapshot never stored";
    CHECK(published.load() == nullptr);
}

TEST_CASE("a_stored_snapshot_is_what_the_next_read_returns", tags) {
    auto lg(make_logger(test_suite));

    atomic_shared_ptr<const snapshot> published;
    published.store(std::make_shared<const snapshot>(snapshot{"first", 1}));

    auto read = published.load();

    BOOST_LOG_SEV(lg, info) << "Reading back the stored revision";
    REQUIRE(read != nullptr);
    CHECK(read->name == "first");
    CHECK(read->revision == 1);
}

TEST_CASE("a_read_survives_the_store_that_replaces_it", tags) {
    auto lg(make_logger(test_suite));

    atomic_shared_ptr<const snapshot> published;
    published.store(std::make_shared<const snapshot>(snapshot{"old", 1}));

    auto reader = published.load();
    published.store(std::make_shared<const snapshot>(snapshot{"new", 2}));

    // The reader holds one whole object, not a view of whatever is current:
    // the store above must not have rewritten what the reader is using.
    BOOST_LOG_SEV(lg, info) << "Checking the reader's snapshot after a store";
    CHECK(reader->name == "old");
    CHECK(reader->revision == 1);
    CHECK(published.load()->name == "new");
    CHECK(published.load()->revision == 2);
}

TEST_CASE("readers_never_see_a_half_published_snapshot", tags) {
    auto lg(make_logger(test_suite));

    constexpr int revisions = 20000;
    atomic_shared_ptr<const snapshot> published;
    published.store(std::make_shared<const snapshot>(snapshot{"rev-0", 0}));

    std::atomic<bool> stop{false};
    std::atomic<int> torn{0};
    std::vector<std::thread> readers;
    for (int r = 0; r < 3; ++r) {
        readers.emplace_back([&] {
            while (!stop.load(std::memory_order_relaxed)) {
                auto s = published.load();
                if (s == nullptr) {
                    torn.fetch_add(1);
                    continue;
                }
                // The name is derived from the revision, so a snapshot that
                // mixes one publisher's name with another's revision is a
                // torn read.
                if (s->name != "rev-" + std::to_string(s->revision))
                    torn.fetch_add(1);
            }
        });
    }

    for (int i = 1; i <= revisions; ++i) {
        published.store(std::make_shared<const snapshot>(snapshot{"rev-" + std::to_string(i), i}));
    }
    stop.store(true, std::memory_order_relaxed);
    for (auto& t : readers)
        t.join();

    BOOST_LOG_SEV(lg, info) << "Torn reads over " << revisions << " revisions: " << torn.load();
    CHECK(torn.load() == 0);
    CHECK(published.load()->revision == revisions);
}
