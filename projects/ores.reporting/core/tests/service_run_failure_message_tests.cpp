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
#include "ores.reporting.core/service/run_failure_message.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

// The decision these pin: a failed run states a summary, not a transcript. The
// rows the log produced are the account of what went wrong, and the run's
// message says how many there are and where a reader finds them.

namespace {

using ores::reporting::service::run_failure_message;
using ores::reporting::service::run_problems;

/** The reason a node states for a failed engine run, transcript and all. */
const std::string node_reason =
    "Engine exited with code 255\n"
    "WARNING  [2026-Nov-03 22:40:07.022279] (OREData/ored/portfolio/portfolio.cpp:103) : Added "
    "trade id 00cf3e0f-29e1-53f9-9e85-eac72dcff6ca type Failed for original trade type "
    "CallableSwap\n";

}

TEST_CASE("a run whose log gave rows states the count and points at the Errors",
          "[reporting][service][failure_message]") {
    const std::string message = run_failure_message(3, node_reason, {.errors = 24, .warnings = 0});

    // The node's verdict is kept, its transcript is not: the rows say that part
    // once, per failure, and a wall of text is what the run is meant to stop
    // showing.
    CHECK(
        message ==
        "The run's compute job failed with outcome 3: Engine exited with code 255. Its log records "
        "24 errors; the rows are in Errors.");
    CHECK(message.find("Added trade id") == std::string::npos);
}

TEST_CASE("a run's summary counts the warnings apart from the errors",
          "[reporting][service][failure_message]") {
    const auto message =
        run_failure_message(3, "Engine exited with code 255", {.errors = 1, .warnings = 2});
    CHECK(message.find("records 1 error and 2 warnings;") != std::string::npos);
}

TEST_CASE("a run whose log gave nothing keeps the node's own account whole",
          "[reporting][service][failure_message]") {
    // The log could not be read, so the node's reason is the only account of
    // what went wrong and nothing may shorten it.
    const auto message = run_failure_message(3, node_reason, {});

    CHECK(message == "The run's compute job failed with outcome 3: " + node_reason);
}

TEST_CASE("a failure with no reason and no rows still states its outcome",
          "[reporting][service][failure_message]") {
    CHECK(run_failure_message(3, {}, {}) == "The run's compute job failed with outcome 3.");
}
