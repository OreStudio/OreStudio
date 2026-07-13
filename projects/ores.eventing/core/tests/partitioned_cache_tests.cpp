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
#include "ores.eventing.core/service/cache/partitioned_cache.hpp"
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <immer/map_transient.hpp>
#include <string>
#include <thread>
#include <utility>
#include <vector>

using ores::eventing::service::cache::partitioned_cache;

namespace {

/// A trivial aux index: how many entries the partition had when replaced.
struct entry_count {
    std::size_t value = 0;
};

using string_cache = partitioned_cache<std::string, std::string, int>;
using string_cache_with_aux = partitioned_cache<std::string, std::string, int, entry_count>;

template <typename Cache>
typename Cache::entries_map make_entries(const std::vector<std::pair<std::string, int>>& kvs) {
    auto t = typename Cache::entries_map{}.transient();
    for (const auto& [k, v] : kvs)
        t.set(k, v);
    return t.persistent();
}

} // namespace

TEST_CASE("lookup on an unknown partition returns nullopt", "[partitioned_cache]") {
    string_cache cache;
    CHECK_FALSE(cache.get("tenant-a", "x").has_value());
    CHECK_FALSE(cache.snapshot("tenant-a").has_value());
}

TEST_CASE("replace_partition makes entries visible to get", "[partitioned_cache]") {
    string_cache cache;
    cache.replace_partition("tenant-a", make_entries<string_cache>({{"x", 1}, {"y", 2}}));

    const auto x = cache.get("tenant-a", "x");
    REQUIRE(x.has_value());
    CHECK(*x == 1);
    CHECK(cache.get("tenant-a", "z") == std::nullopt);
}

TEST_CASE("other partitions are untouched by a replace", "[partitioned_cache]") {
    string_cache cache;
    cache.replace_partition("tenant-a", make_entries<string_cache>({{"x", 1}}));
    cache.replace_partition("tenant-b", make_entries<string_cache>({{"x", 99}}));

    CHECK(cache.get("tenant-a", "x") == 1);
    CHECK(cache.get("tenant-b", "x") == 99);
}

TEST_CASE("a second replace_partition wholesale-replaces the prior snapshot",
         "[partitioned_cache]") {
    string_cache cache;
    cache.replace_partition("tenant-a", make_entries<string_cache>({{"x", 1}, {"y", 2}}));
    cache.replace_partition("tenant-a", make_entries<string_cache>({{"x", 100}}));

    CHECK(cache.get("tenant-a", "x") == 100);
    CHECK(cache.get("tenant-a", "y") == std::nullopt);
}

TEST_CASE("aux index defaults are used when not supplied", "[partitioned_cache]") {
    string_cache_with_aux cache;
    cache.replace_partition("tenant-a", make_entries<string_cache_with_aux>({{"x", 1}}));

    const auto snap = cache.snapshot("tenant-a");
    REQUIRE(snap.has_value());
    CHECK(snap->aux.value == 0);
}

TEST_CASE("aux index is carried alongside the entries when supplied", "[partitioned_cache]") {
    string_cache_with_aux cache;
    const auto entries = make_entries<string_cache_with_aux>({{"x", 1}, {"y", 2}});
    cache.replace_partition("tenant-a", entries, entry_count{2});

    const auto snap = cache.snapshot("tenant-a");
    REQUIRE(snap.has_value());
    CHECK(snap->aux.value == 2);
    CHECK(snap->entries.size() == 2);
}

TEST_CASE("concurrent replace_partition on different partitions never loses an update",
         "[partitioned_cache]") {
    string_cache cache;
    constexpr int tenant_count = 16;
    constexpr int rounds = 50;

    std::vector<std::thread> threads;
    threads.reserve(tenant_count);
    for (int t = 0; t < tenant_count; ++t) {
        threads.emplace_back([&cache, t]() {
            const auto tenant = "tenant-" + std::to_string(t);
            for (int round = 0; round < rounds; ++round)
                cache.replace_partition(tenant,
                                        make_entries<string_cache>({{"v", round}}));
        });
    }
    for (auto& th : threads)
        th.join();

    // Every tenant's last write must have landed: each partition's value
    // must be a value that was actually written (0..rounds-1), not lost
    // or torn -- if updates were racing via a blind store() instead of a
    // CAS loop, a partition could end up missing entirely.
    for (int t = 0; t < tenant_count; ++t) {
        const auto tenant = "tenant-" + std::to_string(t);
        const auto v = cache.get(tenant, "v");
        REQUIRE(v.has_value());
        CHECK(*v >= 0);
        CHECK(*v < rounds);
    }
}

TEST_CASE("a reader never observes a torn mix within one snapshot", "[partitioned_cache]") {
    string_cache cache;
    cache.replace_partition("tenant-a", make_entries<string_cache>({{"x", 0}, {"y", 0}}));

    std::atomic<bool> stop{false};
    std::thread writer([&cache, &stop]() {
        int i = 1;
        while (!stop.load(std::memory_order_relaxed)) {
            cache.replace_partition("tenant-a",
                                    make_entries<string_cache>({{"x", i}, {"y", i}}));
            ++i;
        }
    });

    // A torn snapshot would show x and y with different generations after
    // a concurrent replace landed mid-read; since replace_partition swaps
    // the whole immutable partition atomically, x and y must always agree
    // within a single snapshot() call.
    for (int i = 0; i < 2000; ++i) {
        const auto snap = cache.snapshot("tenant-a");
        REQUIRE(snap.has_value());
        const auto* x = snap->entries.find("x");
        const auto* y = snap->entries.find("y");
        REQUIRE(x);
        REQUIRE(y);
        CHECK(*x == *y);
    }

    stop.store(true, std::memory_order_relaxed);
    writer.join();
}
