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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.platform/concurrency/stoppable_thread.hpp"
#include <atomic>
#include <catch2/catch_template_test_macros.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <future>
#include <string>
#include <thread>

namespace {

using namespace std::chrono_literals;

// A wait this long that returns in a fraction of it was woken.
constexpr auto long_wait = 30s;
constexpr auto woken_within = 2s;

namespace concurrency = ores::platform::concurrency;

/// The portable types, which every platform compiles and only some select.
struct portable_kit {
    using source = concurrency::portable::stop_source;
    using token = concurrency::portable::stop_token;
    using thread = concurrency::portable::jthread;

    static bool wait_for(const token& stop, std::chrono::milliseconds span) {
        return concurrency::portable::wait_for(stop, span);
    }
};

/// The types the platform selects: the standard ones where the library has them.
struct selected_kit {
    using source = concurrency::stop_source;
    using token = concurrency::stop_token;
    using thread = concurrency::jthread;

    static bool wait_for(const token& stop, std::chrono::milliseconds span) {
        return concurrency::wait_for(stop, span);
    }
};

}

TEMPLATE_TEST_CASE("a_wait_returns_false_when_the_span_passes_with_no_stop",
                   "[concurrency][stoppable_thread]",
                   portable_kit,
                   selected_kit) {
    typename TestType::source source;

    const auto started = std::chrono::steady_clock::now();
    CHECK_FALSE(TestType::wait_for(source.get_token(), 50ms));
    CHECK(std::chrono::steady_clock::now() - started >= 50ms);
    CHECK_FALSE(source.stop_requested());
}

TEMPLATE_TEST_CASE("a_wait_returns_at_once_when_a_stop_was_requested_before_it",
                   "[concurrency][stoppable_thread]",
                   portable_kit,
                   selected_kit) {
    typename TestType::source source;
    source.request_stop();

    const auto started = std::chrono::steady_clock::now();
    CHECK(TestType::wait_for(source.get_token(), long_wait));
    CHECK(std::chrono::steady_clock::now() - started < woken_within);
}

TEMPLATE_TEST_CASE("a_stop_wakes_a_wait_that_is_in_progress", "[concurrency][stoppable_thread]", portable_kit, selected_kit) {
    typename TestType::source source;
    auto waiter = std::async(std::launch::async, [token = source.get_token()] {
        const auto started = std::chrono::steady_clock::now();
        const bool stopped = TestType::wait_for(token, long_wait);
        return std::pair{stopped, std::chrono::steady_clock::now() - started};
    });

    // The waiter is in its wait when the stop arrives, or about to be: either way it returns.
    std::this_thread::sleep_for(50ms);
    source.request_stop();

    const auto [stopped, waited] = waiter.get();
    CHECK(stopped);
    CHECK(waited < woken_within);
}

TEMPLATE_TEST_CASE("tokens_of_one_source_share_its_state", "[concurrency][stoppable_thread]", portable_kit, selected_kit) {
    typename TestType::source source;
    const auto first = source.get_token();
    const auto second = source.get_token();

    CHECK_FALSE(first.stop_requested());
    CHECK(source.request_stop());
    CHECK_FALSE(source.request_stop());

    CHECK(first.stop_requested());
    CHECK(second.stop_requested());
}

TEMPLATE_TEST_CASE("a_token_with_no_state_is_never_stopped_and_waits_the_span",
                   "[concurrency][stoppable_thread]",
                   portable_kit,
                   selected_kit) {
    const typename TestType::token none;

    const auto started = std::chrono::steady_clock::now();
    CHECK_FALSE(TestType::wait_for(none, 50ms));
    CHECK(std::chrono::steady_clock::now() - started >= 50ms);
    CHECK_FALSE(none.stop_requested());
    CHECK_FALSE(none.stop_possible());
}

TEMPLATE_TEST_CASE("a_moved_from_source_does_nothing", "[concurrency][stoppable_thread]", portable_kit, selected_kit) {
    typename TestType::source source;
    const typename TestType::source taken = std::move(source);

    CHECK_FALSE(source.request_stop());
    CHECK_FALSE(source.stop_requested());
    CHECK_FALSE(taken.stop_requested());
}

TEMPLATE_TEST_CASE("the_destructor_stops_a_waiting_thread_and_joins_it",
                   "[concurrency][stoppable_thread]",
                   portable_kit,
                   selected_kit) {
    std::atomic<bool> left_the_wait = false;
    const auto started = std::chrono::steady_clock::now();
    {
        typename TestType::thread thread([&left_the_wait](typename TestType::token stop) {
            TestType::wait_for(stop, long_wait);
            left_the_wait = true;
        });
        std::this_thread::sleep_for(50ms);
    }

    CHECK(left_the_wait);
    CHECK(std::chrono::steady_clock::now() - started < woken_within);
}

TEMPLATE_TEST_CASE("request_stop_on_the_thread_reaches_the_callable",
                   "[concurrency][stoppable_thread]",
                   portable_kit,
                   selected_kit) {
    std::promise<bool> answer;
    auto answered = answer.get_future();
    typename TestType::thread thread([&answer](typename TestType::token stop) {
        answer.set_value(TestType::wait_for(stop, long_wait));
    });

    thread.request_stop();

    REQUIRE(answered.wait_for(woken_within) == std::future_status::ready);
    CHECK(answered.get());
    CHECK(thread.get_stop_token().stop_requested());
}

TEMPLATE_TEST_CASE("a_callable_that_takes_no_token_runs_and_joins",
                   "[concurrency][stoppable_thread]",
                   portable_kit,
                   selected_kit) {
    std::atomic<bool> ran = false;
    {
        typename TestType::thread thread([&ran] { ran = true; });
    }

    CHECK(ran);
}

TEMPLATE_TEST_CASE("assigning_over_a_running_thread_stops_and_joins_it_first",
                   "[concurrency][stoppable_thread]",
                   portable_kit,
                   selected_kit) {
    std::atomic<int> finished = 0;
    const auto make = [&finished] {
        return typename TestType::thread([&finished](typename TestType::token stop) {
            TestType::wait_for(stop, long_wait);
            ++finished;
        });
    };

    const auto started = std::chrono::steady_clock::now();
    typename TestType::thread thread = make();
    std::this_thread::sleep_for(50ms);
    thread = make();

    CHECK(finished == 1);
    CHECK(std::chrono::steady_clock::now() - started < woken_within);
}
