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
#include "ores.eventing.core/service/cache/partition_token_cache.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

using ores::eventing::service::cache::partition_token;
using ores::eventing::service::cache::partition_token_cache;

namespace {

using std::chrono::seconds;
using time_point = std::chrono::system_clock::time_point;

/// A minter that names each token after its partition and the exchange count.
struct counting_minter {
    std::vector<std::string>* asked;
    time_point* now;
    seconds lifetime;

    partition_token operator()(const std::string& partition) const {
        asked->push_back(partition);
        return {partition + "#" + std::to_string(asked->size()), *now + lifetime};
    }
};

}

TEST_CASE("each partition gets its own token", "[partition_token_cache]") {
    std::vector<std::string> asked;
    time_point now{};
    partition_token_cache cache(
        counting_minter{&asked, &now, seconds(300)}, seconds(30), [&] { return now; });

    CHECK(cache.get("tenant-a") == "tenant-a#1");
    CHECK(cache.get("tenant-b") == "tenant-b#2");
    CHECK(asked == std::vector<std::string>{"tenant-a", "tenant-b"});
}

TEST_CASE("a fresh token is reused without another exchange", "[partition_token_cache]") {
    std::vector<std::string> asked;
    time_point now{};
    partition_token_cache cache(
        counting_minter{&asked, &now, seconds(300)}, seconds(30), [&] { return now; });

    CHECK(cache.get("tenant-a") == "tenant-a#1");
    now += seconds(200);
    CHECK(cache.get("tenant-a") == "tenant-a#1");
    CHECK(asked.size() == 1);
}

TEST_CASE("a token within the margin of its expiry is exchanged again", "[partition_token_cache]") {
    std::vector<std::string> asked;
    time_point now{};
    partition_token_cache cache(
        counting_minter{&asked, &now, seconds(300)}, seconds(30), [&] { return now; });

    CHECK(cache.get("tenant-a") == "tenant-a#1");
    now += seconds(271);
    CHECK(cache.get("tenant-a") == "tenant-a#2");
}

TEST_CASE("renew exchanges again even when the token held looks fresh", "[partition_token_cache]") {
    std::vector<std::string> asked;
    time_point now{};
    partition_token_cache cache(
        counting_minter{&asked, &now, seconds(300)}, seconds(30), [&] { return now; });

    CHECK(cache.get("tenant-a") == "tenant-a#1");
    CHECK(cache.get("tenant-a", true) == "tenant-a#2");
    CHECK(cache.get("tenant-a") == "tenant-a#2");
}

TEST_CASE("a refused exchange holds nothing, so the next get asks again",
          "[partition_token_cache]") {
    int exchanges = 0;
    bool refuse = true;
    partition_token_cache cache([&](const std::string& partition) {
        ++exchanges;
        return refuse ? partition_token{} :
                        partition_token{partition, std::chrono::system_clock::now() + seconds(300)};
    });

    CHECK(cache.get("tenant-a").empty());
    refuse = false;
    CHECK(cache.get("tenant-a") == "tenant-a");
    CHECK(exchanges == 2);
}

TEST_CASE("the provider hands out the cache's tokens", "[partition_token_cache]") {
    std::vector<std::string> asked;
    time_point now{};
    partition_token_cache cache(
        counting_minter{&asked, &now, seconds(300)}, seconds(30), [&] { return now; });
    const auto provider = cache.provider();

    CHECK(provider("tenant-a", false) == "tenant-a#1");
    CHECK(provider("tenant-a", false) == "tenant-a#1");
    CHECK(provider("tenant-a", true) == "tenant-a#2");
}
