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
#include "ores.marketdata.service/app/drop_oldest_queue.hpp"
#include "ores.marketdata.service/app/observation_batch.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <thread>
#include <vector>

namespace {

const std::string tags("[app][tick_batch]");

using ores::marketdata::domain::market_observation;
using ores::marketdata::service::app::drop_oldest_queue;
using ores::marketdata::service::app::keep_last_per_key;

constexpr auto no_wait = std::chrono::milliseconds(0);

market_observation row(const boost::uuids::uuid& series,
                       std::chrono::system_clock::time_point at,
                       const std::string& value,
                       const std::string& uri = "oresmd://fx/USD?type=quote") {
    static boost::uuids::random_generator gen;
    market_observation o;
    o.id = gen();
    o.series_id = series;
    o.observation_datetime = at;
    o.oresmd_uri = uri;
    o.value = value;
    return o;
}

}

TEST_CASE("a_full_queue_drops_its_oldest_entry_and_counts_the_drop", tags) {
    drop_oldest_queue<int> queue(3);
    for (int i = 1; i <= 5; ++i)
        queue.push(i);

    std::vector<int> out;
    REQUIRE(queue.take_all(out, 10, no_wait) == 3);
    CHECK(out == std::vector<int>{3, 4, 5});
    CHECK(queue.dropped() == 2);
    CHECK(queue.size() == 0);
}

TEST_CASE("a_queue_gives_up_at_most_the_asked_number_of_entries_oldest_first", tags) {
    drop_oldest_queue<int> queue(10);
    for (int i = 1; i <= 6; ++i)
        queue.push(i);

    std::vector<int> first;
    CHECK(queue.take_all(first, 4, no_wait) == 4);
    CHECK(first == std::vector<int>{1, 2, 3, 4});

    std::vector<int> rest;
    CHECK(queue.take_all(rest, 4, no_wait) == 2);
    CHECK(rest == std::vector<int>{5, 6});
}

TEST_CASE("an_empty_queue_returns_nothing_when_the_wait_ends", tags) {
    drop_oldest_queue<int> queue(3);
    std::vector<int> out;
    CHECK(queue.take_all(out, 10, std::chrono::milliseconds(20)) == 0);
    CHECK(out.empty());
}

TEST_CASE("a_push_wakes_a_waiting_worker", tags) {
    drop_oldest_queue<int> queue(3);
    std::vector<int> out;
    std::thread worker([&] { queue.take_all(out, 10, std::chrono::seconds(10)); });
    std::this_thread::sleep_for(std::chrono::milliseconds(50));
    queue.push(7);
    worker.join();
    CHECK(out == std::vector<int>{7});
}

TEST_CASE("closing_a_queue_wakes_a_waiting_worker_with_nothing", tags) {
    drop_oldest_queue<int> queue(3);
    std::vector<int> out;
    std::thread worker([&] { queue.take_all(out, 10, std::chrono::seconds(10)); });
    std::this_thread::sleep_for(std::chrono::milliseconds(50));
    queue.close();
    worker.join();
    CHECK(out.empty());
}

TEST_CASE("a_batch_keeps_the_last_row_of_each_key_and_the_order_of_the_rest", tags) {
    boost::uuids::random_generator gen;
    const auto series_a = gen();
    const auto series_b = gen();
    const auto t0 = std::chrono::system_clock::now();
    const auto t1 = t0 + std::chrono::milliseconds(5);

    std::vector<market_observation> rows;
    rows.push_back(row(series_a, t0, "first"));
    rows.push_back(row(series_b, t0, "other_series"));
    rows.push_back(row(series_a, t0, "second"));
    rows.push_back(row(series_a, t1, "other_time"));

    const auto kept = keep_last_per_key(std::move(rows));
    REQUIRE(kept.size() == 3);
    CHECK(kept[0].value == "other_series");
    CHECK(kept[1].value == "second");
    CHECK(kept[2].value == "other_time");
}

TEST_CASE("rows_of_one_series_and_time_with_different_coordinates_are_all_kept", tags) {
    boost::uuids::random_generator gen;
    const auto series = gen();
    const auto at = std::chrono::system_clock::now();

    std::vector<market_observation> rows;
    rows.push_back(row(series, at, "3m", "oresmd://ir/USD?type=quote&tenor=3M"));
    rows.push_back(row(series, at, "6m", "oresmd://ir/USD?type=quote&tenor=6M"));

    CHECK(keep_last_per_key(std::move(rows)).size() == 2);
}
