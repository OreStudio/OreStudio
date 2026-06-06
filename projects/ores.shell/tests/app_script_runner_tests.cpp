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
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/script_runner.hpp"
#include "ores.logging/make_logger.hpp"
#include <catch2/catch_test_macros.hpp>
#include <sstream>
#include <vector>

namespace {

const std::string_view test_suite("ores.shell.tests");
const std::string tags("[app]");

}

using ores::shell::app::command_feedback;
using ores::shell::app::fail;
using ores::shell::app::run_script;
using namespace ores::logging;

TEST_CASE("command_feedback_reset_clears_failure", tags) {
    auto lg(make_logger(test_suite));

    command_feedback::mark_failure();
    CHECK(ores::shell::app::command_feedback::failed());
    command_feedback::reset();
    CHECK_FALSE(command_feedback::failed());
}

TEST_CASE("fail_marks_failure_and_prefixes_message", tags) {
    auto lg(make_logger(test_suite));

    command_feedback::reset();
    std::ostringstream out;
    fail(out) << "boom" << std::endl;
    CHECK(command_feedback::failed());
    CHECK(out.str() == "✗ boom\n");
    command_feedback::reset();
}

TEST_CASE("run_script_executes_commands_and_skips_noise", tags) {
    auto lg(make_logger(test_suite));
    command_feedback::reset();

    std::istringstream in("# comment\n"
                          "\n"
                          "  one  \n"
                          "\t# indented comment\n"
                          "two\n");
    std::vector<std::string> fed;
    std::ostringstream out;
    auto r = run_script(in, [&](const std::string& c) { fed.push_back(c); },
                        out, false);

    CHECK_FALSE(r.aborted);
    CHECK(r.executed == 2);
    CHECK(fed == std::vector<std::string>{"one", "two"});
}

TEST_CASE("run_script_aborts_on_first_failure_by_default", tags) {
    auto lg(make_logger(test_suite));
    command_feedback::reset();

    std::istringstream in("good\n"
                          "bad\n"
                          "never\n");
    std::vector<std::string> fed;
    std::ostringstream out;
    auto r = run_script(in,
                        [&](const std::string& c) {
                            fed.push_back(c);
                            if (c == "bad")
                                command_feedback::mark_failure();
                        },
                        out, false);

    CHECK(r.aborted);
    CHECK(r.aborted_line == 2);
    CHECK(r.aborted_command == "bad");
    CHECK(r.executed == 2);
    CHECK(fed == std::vector<std::string>{"good", "bad"});
}

TEST_CASE("run_script_continue_on_error_keeps_going", tags) {
    auto lg(make_logger(test_suite));
    command_feedback::reset();

    std::istringstream in("good\n"
                          "bad\n"
                          "after\n");
    std::vector<std::string> fed;
    std::ostringstream out;
    auto r = run_script(in,
                        [&](const std::string& c) {
                            fed.push_back(c);
                            if (c == "bad")
                                command_feedback::mark_failure();
                        },
                        out, true);

    CHECK_FALSE(r.aborted);
    CHECK(r.executed == 3);
    CHECK(fed == std::vector<std::string>{"good", "bad", "after"});
}

TEST_CASE("run_script_nested_failure_propagates", tags) {
    auto lg(make_logger(test_suite));
    command_feedback::reset();

    // Outer script feeds a command which itself runs an inner script
    // whose failure aborts it; the inner abort marks failure, which
    // the outer runner observes after the feed returns.
    std::istringstream outer_in("run-inner\n"
                                "never\n");
    std::ostringstream out;
    auto inner = [&](const std::string&) {
        std::istringstream inner_in("inner-bad\n");
        auto r = run_script(inner_in,
                            [](const std::string&) {
                                command_feedback::mark_failure();
                            },
                            out, false);
        CHECK(r.aborted);
        // As load does: leave the failure flag set on abort.
        command_feedback::mark_failure();
    };

    auto r = run_script(outer_in, inner, out, false);
    CHECK(r.aborted);
    CHECK(r.aborted_line == 1);
    CHECK(r.aborted_command == "run-inner");
}
