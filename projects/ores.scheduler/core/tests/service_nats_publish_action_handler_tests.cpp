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
#include "ores.nats/service/nats_client.hpp"
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

// The NATS-publish handler is how a report job reaches the reporting service:
// the action payload names the subject, and the body carries the definition,
// its tenant and the run. A job whose payload the handler cannot read must not
// take the scheduler loop down, so the failure path returns rather than throws.
//
// The report-trigger case needs the fleet: it asserts that a refusal comes
// back, which only happens if the reporting service answers.

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

// A client with no token provider, so an authenticated request presents no
// bearer. The refusal tests rely on that.
ores::nats::service::nats_client make_unauthenticated_client(ores::nats::service::client& nats) {
    return ores::nats::service::nats_client(nats, [](bool) { return std::string{}; });
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
    auto svc_nats = make_unauthenticated_client(nats);
    nats_publish_action_handler handler(nats, svc_nats);

    CHECK(handler.action_type() == "nats_publish");
}

TEST_CASE("nats_publish_action_handler refuses a payload it cannot read", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    ores::nats::service::client nats(ores::testing::make_nats_options());
    auto svc_nats = make_unauthenticated_client(nats);
    nats_publish_action_handler handler(nats, svc_nats);

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
    auto svc_nats = make_unauthenticated_client(nats);
    nats_publish_action_handler handler(nats, svc_nats);

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

    auto svc_nats = make_unauthenticated_client(nats);
    nats_publish_action_handler handler(nats, svc_nats);

    // No report definition, so this is a notification nothing answers: it is
    // published and the job succeeds.
    const auto job = make_job(R"({"subject":"ores.scheduler.action_handler_test"})");
    const action_context ctx{job, h.context(), 42};

    boost::asio::io_context io;
    std::expected<void, std::string> result;
    ores::testing::run_coroutine_test(
        io, [&]() -> boost::asio::awaitable<void> { result = co_await handler.execute(ctx); });

    CHECK(result.has_value());
}

TEST_CASE("nats_publish_action_handler fails the job when the trigger is refused", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    REQUIRE(nats.is_connected());

    // The request presents no bearer, so the reporting service refuses it. The
    // refusal has to reach the job: before this change it was discarded and the
    // job was recorded as succeeded while no report instance existed.
    auto svc_nats = make_unauthenticated_client(nats);
    nats_publish_action_handler handler(nats, svc_nats);

    const auto job = make_job(R"({"subject":"reporting.v1.ops.trigger_report_instance",)"
                              R"("report_definition_id":"00000000-0000-0000-0000-000000000000",)"
                              R"("tenant_id":"00000000-0000-0000-0000-000000000000"})");
    const action_context ctx{job, h.context(), 7};

    boost::asio::io_context io;
    std::expected<void, std::string> result;
    ores::testing::run_coroutine_test(
        io, [&]() -> boost::asio::awaitable<void> { result = co_await handler.execute(ctx); });

    REQUIRE_FALSE(result.has_value());
    CHECK(result.error().find("refused the trigger") != std::string::npos);
}
