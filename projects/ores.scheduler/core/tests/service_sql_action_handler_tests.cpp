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
#include "ores.scheduler.api/domain/job_definition.hpp"
#include "ores.scheduler.core/service/sql_action_handler.hpp"
#include "ores.testing/run_coroutine_test.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <expected>
#include <string>
#include <string_view>

// The action handlers are what a fired job actually does. The SQL handler runs
// the job's command against the database and reports the database's own
// refusal rather than throwing, so a failing job is recorded as a failed
// instance instead of taking the scheduler loop down with it.

namespace {

const std::string_view test_suite("scheduler.tests");
const std::string tags("[action][sql]");

ores::scheduler::domain::job_definition make_job(const std::string& command) {
    ores::scheduler::domain::job_definition job;
    job.job_name = "action_handler_test_job";
    job.command = command;
    job.action_type = "execute_sql";
    return job;
}

}

using namespace ores::logging;
using ores::scheduler::domain::job_definition;
using ores::scheduler::service::action_context;
using ores::scheduler::service::sql_action_handler;
using ores::testing::scoped_database_helper;

TEST_CASE("sql_action_handler claims the execute_sql action type", tags) {
    auto lg(make_logger(test_suite));

    sql_action_handler handler;
    CHECK(handler.action_type() == "execute_sql");
}

TEST_CASE("sql_action_handler runs the job's command", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    const auto job = make_job("select 1");
    const action_context ctx{job, h.context(), 0};

    sql_action_handler handler;
    boost::asio::io_context io;
    std::expected<void, std::string> result;
    ores::testing::run_coroutine_test(
        io, [&]() -> boost::asio::awaitable<void> { result = co_await handler.execute(ctx); });

    CHECK(result.has_value());
}

TEST_CASE("sql_action_handler reports a bad command instead of throwing", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    const auto job = make_job("this is not sql");
    const action_context ctx{job, h.context(), 0};

    sql_action_handler handler;
    boost::asio::io_context io;
    std::expected<void, std::string> result;
    ores::testing::run_coroutine_test(
        io, [&]() -> boost::asio::awaitable<void> { result = co_await handler.execute(ctx); });

    // The distinction matters: an exception here would propagate out of the
    // scheduler loop's co_spawn and end the loop, so a job with a broken
    // command would stop every other job from firing. The handler must return
    // the failure so the loop can record it against the instance.
    REQUIRE_FALSE(result.has_value());
    CHECK_FALSE(result.error().empty());
}
