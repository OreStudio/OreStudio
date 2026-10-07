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
#include "ores.synthetic.api/feeds/tick_clock.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.synthetic.tests");
const std::string tags("[tick_clock]");

// One tick per millisecond, so a three-tick case is fast but still exercises
// the real period/sleep path.
constexpr double fast = 3600000.0;

}

using ores::synthetic::feed::tick_clock;
using namespace ores::logging;

TEST_CASE("tick_clock runs the body once per period and counts every batch", tags) {
    auto lg(make_logger(test_suite));

    tick_clock clock(fast);
    std::vector<std::uint64_t> counts;
    std::vector<std::string> summaries;
    int calls = 0;

    clock.run(
        [&] {
            ++calls;
            if (calls == 3)
                clock.stop();
            return std::string("tick") + std::to_string(calls);
        },
        [&](std::uint64_t n, const std::string& summary) {
            counts.push_back(n);
            summaries.push_back(summary);
        },
        [](const std::exception&) { FAIL("the tick body did not throw"); });

    CHECK(calls == 3);
    CHECK(clock.publish_count() == 3);
    CHECK(counts == std::vector<std::uint64_t>{1, 2, 3});
    CHECK(summaries == std::vector<std::string>{"tick1", "tick2", "tick3"});
}

TEST_CASE("tick_clock skips a throwing tick and keeps counting from the next one", tags) {
    auto lg(make_logger(test_suite));

    tick_clock clock(fast);
    std::vector<std::uint64_t> counts;
    std::vector<std::string> failures;
    int calls = 0;

    clock.run(
        [&] {
            ++calls;
            if (calls == 1)
                throw std::runtime_error("first tick failed");
            if (calls == 3)
                clock.stop();
            return std::string("ok");
        },
        [&](std::uint64_t n, const std::string&) { counts.push_back(n); },
        [&](const std::exception& ex) { failures.push_back(ex.what()); });

    CHECK(calls == 3);
    CHECK(clock.publish_count() == 2);
    CHECK(counts == std::vector<std::uint64_t>{1, 2});
    CHECK(failures == std::vector<std::string>{"first tick failed"});
}

TEST_CASE("tick_clock stopped before run emits nothing", tags) {
    auto lg(make_logger(test_suite));

    tick_clock clock(fast);
    clock.stop();

    int calls = 0;
    clock.run(
        [&] {
            ++calls;
            return std::string{};
        },
        [](std::uint64_t, const std::string&) { FAIL("a stopped clock published"); },
        [](const std::exception&) { FAIL("a stopped clock failed"); });

    CHECK(calls == 0);
    CHECK(clock.publish_count() == 0);
}
