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
#include "ores.ore.core/log/engine_run_verdict.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>
#include <vector>

namespace {

const std::string tags("[ore][log][engine_run_verdict]");

using ores::ore::log::judge_engine_run;

/** One line in the engine's own format. */
std::string line(std::string_view level, const std::string& message) {
    return std::string(level) +
           "    [2026-Oct-08 12:00:00.000000]    (ore/oreapp.cpp:42) : " + message + "\n";
}

}

TEST_CASE("a run that wrote analytics is accepted", tags) {
    const auto outcome = judge_engine_run({"log.txt", "portfolio.xml", "reports"}, "");

    CHECK(outcome.succeeded);
    CHECK(outcome.failure.empty());
}

TEST_CASE("a run that wrote only its log is refused", tags) {
    const auto outcome = judge_engine_run({"log.txt"}, "");

    CHECK_FALSE(outcome.succeeded);
    CHECK(outcome.failure == "The engine produced no analytics.");
}

TEST_CASE("an empty output directory is refused", tags) {
    const auto outcome = judge_engine_run({}, "");

    CHECK_FALSE(outcome.succeeded);
    CHECK(outcome.failure == "The engine produced no analytics.");
}

TEST_CASE("the engine's own error reaches the refusal", tags) {
    const auto log =
        line("DATA", "starting up") + line("ERROR",
                                           "Error in ORE analytics: error opening file "
                                           "Input/market_20160205_flat.txt");

    const auto outcome = judge_engine_run({"log.txt"}, log);

    CHECK_FALSE(outcome.succeeded);
    CHECK(outcome.failure ==
          "The engine produced no analytics. The engine reported:\n"
          "  Error in ORE analytics: error opening file Input/market_20160205_flat.txt");
}

TEST_CASE("an error does not fail a run that still wrote analytics", tags) {
    const auto log = line("ERROR", "a leg priced with a fallback curve");

    const auto outcome = judge_engine_run({"log.txt", "portfolio.xml"}, log);

    CHECK(outcome.succeeded);
    CHECK(outcome.failure.empty());
}

TEST_CASE("a refusal says how many errors it left out", tags) {
    std::string log;
    for (int i = 1; i <= 7; ++i)
        log += line("ERROR", "trade " + std::to_string(i) + " would not price");

    const auto outcome = judge_engine_run({"log.txt"}, log);

    CHECK_FALSE(outcome.succeeded);
    CHECK(outcome.failure ==
          "The engine produced no analytics. The engine reported 7 errors; the last 5 are:\n"
          "  trade 3 would not price\n"
          "  trade 4 would not price\n"
          "  trade 5 would not price\n"
          "  trade 6 would not price\n"
          "  trade 7 would not price");
}

TEST_CASE("a log with no error in it says so", tags) {
    const auto log = line("DATA", "loading inputs") + line("NOTICE", "building portfolio") +
                     line("WARNING", "a series was quoted stale");

    const auto outcome = judge_engine_run({"log.txt"}, log);

    CHECK_FALSE(outcome.succeeded);
    CHECK(outcome.failure == "The engine produced no analytics. It logged 3 lines and no error.");
}
