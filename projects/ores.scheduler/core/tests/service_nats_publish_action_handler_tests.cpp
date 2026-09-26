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
#include "ores.nats/service/client.hpp"
#include "ores.scheduler.api/domain/job_definition.hpp"
#include "ores.scheduler.core/service/nats_publish_action_handler.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/run_coroutine_test.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <expected>
#include <string>
#include <string_view>

// The NATS-publish handler is how a report job reaches the rest of the tree:
// the action payload names the subject, and the body carries the instance id.
// A job whose payload the handler cannot read must not take the scheduler loop
// down, so the failure path returns rather than throws.

namespace {

const std::string_view test_suite("scheduler.tests");
const std::string tags("[action][nats]");

ores::scheduler::domain::job_definition make_job(const std::string& payload) {
    ores::scheduler::domain::job_definition job;
    job.job_name = "nats_action_handler_test_job";
    job.command = "";
    job.action_type = "nats_publish";
    job.action_payload = payload;
    return job;
}

}

using namespace ores::logging;
using ores::scheduler::domain::job_definition;
using ores::scheduler::service::action_context;
using ores::scheduler::service::nats_publish_action_handler;
using ores::testing::scoped_database_helper;

TEST_CASE("nats_publish_action_handler claims the nats_publish action type", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats_publish_action_handler handler(nats);

    CHECK(handler.action_type() == "nats_publish");
}

TEST_CASE("nats_publish_action_handler refuses a payload it cannot read", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats_publish_action_handler handler(nats);

    const auto job = make_job("this is not json");
    const action_context ctx{job, h.context(), 0};

    boost::asio::io_context io;
    std::expected<void, std::string> result;
    ores::testing::run_coroutine_test(
        io, [&]() -> boost::asio::awaitable<void> { result = co_await handler.execute(ctx); });

    REQUIRE_FALSE(result.has_value());
    CHECK_FALSE(result.error().empty());
}

TEST_CASE("nats_publish_action_handler refuses a payload with no subject", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats_publish_action_handler handler(nats);

    // Valid JSON, and still unsendable: without a subject there is nothing to
    // publish to, and publishing to the empty subject would be worse than
    // failing.
    const auto job = make_job(R"({"report_definition_id":"x"})");
    const action_context ctx{job, h.context(), 0};

    boost::asio::io_context io;
    std::expected<void, std::string> result;
    ores::testing::run_coroutine_test(
        io, [&]() -> boost::asio::awaitable<void> { result = co_await handler.execute(ctx); });

    REQUIRE_FALSE(result.has_value());
    CHECK(result.error().find("subject") != std::string::npos);
}

TEST_CASE("nats_publish_action_handler publishes to the payload's subject", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    REQUIRE(nats.is_connected());

    nats_publish_action_handler handler(nats);

    const auto job = make_job(R"({"subject":"ores.scheduler.action_handler_test"})");
    const action_context ctx{job, h.context(), 42};

    boost::asio::io_context io;
    std::expected<void, std::string> result;
    ores::testing::run_coroutine_test(
        io, [&]() -> boost::asio::awaitable<void> { result = co_await handler.execute(ctx); });

    CHECK(result.has_value());
}
