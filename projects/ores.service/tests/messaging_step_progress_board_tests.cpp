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
#include "ores.service/messaging/step_progress_board.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::service::messaging::step_progress_board;
using ores::service::messaging::workflow_step_context;

TEST_CASE("a_step_with_no_report_has_no_progress", "[service][step_progress]") {
    CHECK(!step_progress_board::instance().latest("no-such-step").has_value());
}

TEST_CASE("a_step_context_reports_its_latest_progress", "[service][step_progress]") {
    const workflow_step_context wf{
        .step_id = "step-reports", .instance_id = "run", .tenant_id = "tenant", .nats = nullptr};
    wf.progress("trades", 10, 600);
    wf.progress("trades", 140, 600);

    const auto latest = step_progress_board::instance().latest("step-reports");
    REQUIRE(latest.has_value());
    CHECK(latest->phase == "trades");
    CHECK(latest->done == 140);
    CHECK(latest->total == 600);
    step_progress_board::instance().forget("step-reports");
}

TEST_CASE("a_forgotten_step_has_no_progress", "[service][step_progress]") {
    auto& board = step_progress_board::instance();
    board.report("step-forgotten", {.phase = "passwords", .done = 8, .total = 74});
    board.forget("step-forgotten");
    CHECK(!board.latest("step-forgotten").has_value());
}

TEST_CASE("steps_report_progress_apart", "[service][step_progress]") {
    auto& board = step_progress_board::instance();
    board.report("step-a", {.phase = "a", .done = 1, .total = 2});
    board.report("step-b", {.phase = "b", .done = std::nullopt, .total = std::nullopt});

    REQUIRE(board.latest("step-a").has_value());
    CHECK(board.latest("step-a")->phase == "a");
    REQUIRE(board.latest("step-b").has_value());
    CHECK(board.latest("step-b")->phase == "b");
    board.forget("step-a");
    board.forget("step-b");
}
