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
#include "ores.marketdata.service/app/batch_size.hpp"
#include "ores.marketdata.service/app/drop_oldest_queue.hpp"
#include "ores.marketdata.service/app/store_backoff.hpp"
#include "ores.marketdata.service/app/tick_age.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <thread>
#include <vector>

namespace {

const std::string tags("[app][tick_batch]");

using ores::marketdata::service::app::drop_oldest_queue;
using ores::marketdata::messaging::market_tick;
using ores::marketdata::service::app::erase_stale;
using ores::marketdata::service::app::next_batch_ticks;
using ores::marketdata::service::app::store_backoff;

constexpr auto no_wait = std::chrono::milliseconds(0);

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

TEST_CASE("a_tick_older_than_the_limit_is_removed_and_the_others_keep_their_order", tags) {
    using namespace std::chrono;
    const auto now = system_clock::now();
    const auto tick_at = [&](milliseconds age, const std::string& value) {
        market_tick t;
        t.observation_time = now - age;
        t.value = value;
        return t;
    };

    std::vector<market_tick> ticks;
    ticks.push_back(tick_at(seconds(10), "old"));
    ticks.push_back(tick_at(seconds(1), "recent"));
    ticks.push_back(tick_at(-seconds(2), "future"));
    ticks.push_back(tick_at(seconds(6), "also_old"));
    ticks.push_back(tick_at(seconds(5), "at_the_limit"));

    CHECK(erase_stale(ticks, now, seconds(5)) == 2);
    REQUIRE(ticks.size() == 3);
    CHECK(ticks[0].value == "recent");
    CHECK(ticks[1].value == "future");
    CHECK(ticks[2].value == "at_the_limit");
}

TEST_CASE("a_store_that_is_up_may_be_tried_at_any_time", tags) {
    const store_backoff backoff;
    CHECK_FALSE(backoff.down());
    CHECK(backoff.may_try(store_backoff::clock::now()));
}

TEST_CASE("a_failed_store_is_left_alone_until_its_wait_ends", tags) {
    using namespace std::chrono;
    store_backoff backoff(seconds(1), seconds(30));
    const auto t0 = store_backoff::clock::now();

    CHECK(backoff.note_failure(t0));
    CHECK(backoff.down());
    CHECK_FALSE(backoff.may_try(t0 + milliseconds(999)));
    CHECK(backoff.may_try(t0 + seconds(1)));
}

TEST_CASE("the_wait_doubles_at_each_failure_up_to_the_longest_wait", tags) {
    using namespace std::chrono;
    store_backoff backoff(seconds(1), seconds(4));
    auto now = store_backoff::clock::now();

    CHECK(backoff.note_failure(now));
    now += seconds(1);
    CHECK_FALSE(backoff.note_failure(now));
    CHECK_FALSE(backoff.may_try(now + milliseconds(1999)));
    CHECK(backoff.may_try(now + seconds(2)));

    now += seconds(2);
    backoff.note_failure(now);
    CHECK_FALSE(backoff.may_try(now + milliseconds(3999)));
    CHECK(backoff.may_try(now + seconds(4)));

    now += seconds(4);
    backoff.note_failure(now);
    CHECK_FALSE(backoff.may_try(now + milliseconds(3999)));
    CHECK(backoff.may_try(now + seconds(4)));
}

TEST_CASE("a_success_ends_the_outage_and_restores_the_first_wait", tags) {
    using namespace std::chrono;
    store_backoff backoff(seconds(1), seconds(30));
    const auto t0 = store_backoff::clock::now();
    backoff.note_failure(t0);
    backoff.note_failure(t0 + seconds(1));

    CHECK(backoff.note_success());
    CHECK_FALSE(backoff.down());
    CHECK_FALSE(backoff.note_success());

    CHECK(backoff.note_failure(t0 + seconds(10)));
    CHECK(backoff.may_try(t0 + seconds(11)));
}

TEST_CASE("a_fast_batch_grows_the_next_one_and_a_slow_batch_shrinks_it", tags) {
    // 500 ticks in 250 ms is 2,000 ticks in a second.
    CHECK(next_batch_ticks(500, 250, 1000, 50, 20000) == 2000);
    // 1,000 ticks in 4,000 ms is 250 ticks in a second.
    CHECK(next_batch_ticks(1000, 4000, 1000, 50, 20000) == 250);
}

TEST_CASE("the_next_batch_stays_between_the_lowest_and_the_highest_size", tags) {
    CHECK(next_batch_ticks(1, 10, 1000, 50, 20000) == 100);
    CHECK(next_batch_ticks(1, 1000, 1000, 50, 20000) == 50);
    CHECK(next_batch_ticks(5000, 1, 1000, 50, 20000) == 20000);
}

TEST_CASE("a_batch_that_took_no_time_does_not_divide_by_zero", tags) {
    CHECK(next_batch_ticks(100, 0, 1000, 50, 20000) == 20000);
}
