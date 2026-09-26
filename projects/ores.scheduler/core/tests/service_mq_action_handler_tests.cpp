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
#include "ores.scheduler.core/service/mq_action_handler.hpp"
#include "ores.testing/run_coroutine_test.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/asio/io_context.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <expected>
#include <string>
#include <string_view>

// The message-queue handler sends through the database's own send function. Its
// payload names a queue, a message type and a body; everything else is the
// database's decision. Like the SQL handler, a failure must come back as a
// value: an exception here would end the scheduler loop.

namespace {

const std::string_view test_suite("scheduler.tests");
const std::string tags("[action][mq]");

ores::scheduler::domain::job_definition make_job(const std::string& payload) {
    ores::scheduler::domain::job_definition job;
    job.job_name = "mq_action_handler_test_job";
    job.command = "";
    job.action_type = "send_mq_message";
    job.action_payload = payload;
    return job;
}

}

using namespace ores::logging;
using ores::scheduler::domain::job_definition;
using ores::scheduler::service::action_context;
using ores::scheduler::service::mq_action_handler;
using ores::testing::scoped_database_helper;

TEST_CASE("mq_action_handler claims the send_mq_message action type", tags) {
    auto lg(make_logger(test_suite));

    mq_action_handler handler;
    CHECK(handler.action_type() == "send_mq_message");
}

TEST_CASE("mq_action_handler refuses a payload it cannot read", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    mq_action_handler handler;

    const auto job = make_job("this is not json");
    const action_context ctx{job, h.context(), 0};

    boost::asio::io_context io;
    std::expected<void, std::string> result;
    ores::testing::run_coroutine_test(
        io, [&]() -> boost::asio::awaitable<void> { result = co_await handler.execute(ctx); });

    REQUIRE_FALSE(result.has_value());
    CHECK_FALSE(result.error().empty());
}

TEST_CASE("mq_action_handler refuses a payload with no queue", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    mq_action_handler handler;

    const auto job = make_job(R"({"message_type":"scheduler.test"})");
    const action_context ctx{job, h.context(), 0};

    boost::asio::io_context io;
    std::expected<void, std::string> result;
    ores::testing::run_coroutine_test(
        io, [&]() -> boost::asio::awaitable<void> { result = co_await handler.execute(ctx); });

    REQUIRE_FALSE(result.has_value());
    CHECK(result.error().find("queue_id") != std::string::npos);
}

TEST_CASE("mq_action_handler reports the database's refusal for an unknown queue", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    mq_action_handler handler;

    // Well-formed, and addressed to a queue that does not exist: the send
    // function is the authority on that, and its refusal must arrive as a
    // value. A random id keeps the case independent of whatever queues the
    // environment happens to hold.
    const auto unknown_queue = boost::uuids::to_string(boost::uuids::random_generator{}());
    const auto job =
        make_job(R"({"queue_id":")" + unknown_queue + R"(","message_type":"scheduler.test"})");
    const action_context ctx{job, h.context(), 0};

    boost::asio::io_context io;
    std::expected<void, std::string> result;
    ores::testing::run_coroutine_test(
        io, [&]() -> boost::asio::awaitable<void> { result = co_await handler.execute(ctx); });

    REQUIRE_FALSE(result.has_value());
    CHECK_FALSE(result.error().empty());
}
