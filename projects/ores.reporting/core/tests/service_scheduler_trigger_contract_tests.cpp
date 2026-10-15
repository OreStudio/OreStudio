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
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.scheduler.api/domain/job_definition.hpp"
#include "ores.scheduler.core/service/nats_publish_action_handler.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/run_coroutine_test.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/asio/io_context.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <expected>
#include <span>
#include <string>
#include <string_view>
#include <vector>

// The scheduler's nats_publish action carries the body reporting decodes as
// trigger_report_instance_request, and it builds that body itself: it names the
// subject in the job's action_payload, so it cannot call the reporting client
// code that knows the request type. This is the guard for that seam, and it
// reads the seam from the end that broke: it runs the scheduler's own handler
// and decodes the bytes the handler actually published as the production
// request type, rather than as a copy of the struct shaped to match.
//
// The codec is positional, so a field the request type declares and the
// scheduler does not send fails the whole decode rather than that field alone.
// That is the failure this pins: the request type grew a kind field, the
// scheduler still sent three, the reporting handler answered bad_request, and
// every scheduled firing was dropped for a day. Adding a field to
// projects/ores.reporting/modeling/ores.reporting.operations.org and
// regenerating fails this test.

namespace {

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[contract][trigger][integration]");

// The subject the test's stand-in answers on.
//
// Deliberately not the production subject: the live reporting service
// queue-subscribes that one, so a request published there goes to the fleet
// rather than to this test. What is under test is the scheduler's payload, not
// the subject it is published to.
constexpr std::string_view test_subject = "reporting.v1.ops.trigger_report_instance.contract_test";

// A body shaped as the answering operation's response, carrying the outcome the
// reporting service returns when it admits the run.
struct admitted_response {
    ores::utility::domain::result result;
    std::string report_instance_id;
};

ores::scheduler::domain::job_definition make_publish_job(const std::string& payload) {
    ores::scheduler::domain::job_definition job;
    job.job_name = "scheduler_trigger_contract_test_job";
    job.command = "";
    job.action_type = "nats_publish";
    job.action_payload = payload;
    return job;
}

// A client with no token provider, so an authenticated request presents no
// bearer. What the handler owes here is the body it published, not a session.
ores::nats::service::nats_client make_unauthenticated_client(ores::nats::service::client& nats) {
    return ores::nats::service::nats_client(nats, [](bool) { return std::string{}; });
}

}

using ores::reporting::messaging::trigger_report_instance_request;
using ores::testing::scoped_database_helper;

TEST_CASE("what the scheduler publishes decodes as the reporting trigger request", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    REQUIRE(nats.is_connected());

    const auto definition_id =
        boost::uuids::string_generator()("11111111-1111-1111-1111-111111111111");
    const auto tenant_id = boost::uuids::string_generator()("22222222-2222-2222-2222-222222222222");

    std::vector<std::byte> published;
    auto responder = nats.subscribe(test_subject, [&nats, &published](ores::nats::message msg) {
        published = msg.data;
        nats.publish(msg.reply_subject, ores::nats::default_wire_codec().encode(admitted_response{}));
    });

    auto svc_nats = make_unauthenticated_client(nats);
    ores::scheduler::service::nats_publish_action_handler handler(nats, svc_nats);

    const auto payload = std::string(R"({"subject":")") + std::string(test_subject) +
                         R"(","report_definition_id":"11111111-1111-1111-1111-111111111111",)" +
                         R"("tenant_id":"22222222-2222-2222-2222-222222222222"})";
    const auto job = make_publish_job(payload);
    const ores::scheduler::service::action_context ctx{job, h.context(), 4242};

    boost::asio::io_context io;
    std::expected<void, std::string> result;
    ores::testing::run_coroutine_test(
        io, [&]() -> boost::asio::awaitable<void> { result = co_await handler.execute(ctx); });

    INFO("the handler refused the trigger: " << (result ? std::string("<none>") : result.error()));
    REQUIRE(result.has_value());
    REQUIRE_FALSE(published.empty());

    const auto decoded = ores::nats::default_wire_codec().decode<trigger_report_instance_request>(
        std::span<const std::byte>(published));
    INFO("decoding the scheduler's published payload failed: "
         << (decoded ? std::string("<none>") : decoded.error().what()));
    REQUIRE(decoded.has_value());

    // The fields the scheduler is the only source of: the definition and tenant
    // it read from the action payload, and the firing it is reporting.
    CHECK(decoded->report_definition_id == definition_id);
    CHECK(decoded->tenant_id == tenant_id);
    CHECK(decoded->job_instance_id == 4242);
    CHECK(decoded->kind == "production");
}
