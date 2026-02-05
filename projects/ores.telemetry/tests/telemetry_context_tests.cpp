/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.telemetry/domain/telemetry_context.hpp"
#include "ores.telemetry/domain/semantic_conventions.hpp"
#include "ores.telemetry/generators/trace_id_generator.hpp"
#include "ores.telemetry/generators/span_id_generator.hpp"

#include <catch2/catch_test_macros.hpp>
#include <thread>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.telemetry.tests");
const std::string tags("[telemetry_context]");

using namespace ores::telemetry::domain;
using namespace ores::telemetry::generators;

span_context create_test_context() {
    trace_id_generator trace_gen;
    span_id_generator span_gen;

    span_context ctx;
    ctx.trace = trace_gen();
    ctx.span = span_gen();
    ctx.trace_flags = 0x01;
    return ctx;
}

std::shared_ptr<resource> create_test_resource() {
    return std::make_shared<resource>(
        resource::from_environment("test.service", "1.0.0"));
}

}

using namespace ores::logging;

TEST_CASE("telemetry_context_is_valid_when_properly_constructed", tags) {
    auto lg(make_logger(test_suite));

    auto ctx = create_test_context();
    auto res = create_test_resource();
    telemetry_context tctx(ctx, res);

    REQUIRE(tctx.is_valid());
    REQUIRE(tctx.context().is_valid());
    BOOST_LOG_SEV(lg, debug) << "Created telemetry context with trace: "
                             << tctx.get_trace_id().to_hex();
}

TEST_CASE("telemetry_context_start_span_creates_child", tags) {
    auto lg(make_logger(test_suite));

    auto res = create_test_resource();
    auto parent_ctx = telemetry_context::create_root(res);

    auto [child_ctx, child_span] = parent_ctx.start_span("test_operation");

    // Child should have same trace_id
    REQUIRE(child_ctx.get_trace_id() == parent_ctx.get_trace_id());

    // Child should have different span_id
    REQUIRE(child_ctx.get_span_id() != parent_ctx.get_span_id());

    // Child span should have parent set correctly
    REQUIRE(child_span.parent_span_id.has_value());
    REQUIRE(child_span.parent_span_id.value() == parent_ctx.get_span_id());

    // Child span should have correct name
    REQUIRE(child_span.name == "test_operation");

    // Child span should have start time set
    REQUIRE(child_span.start_time.time_since_epoch().count() > 0);

    BOOST_LOG_SEV(lg, debug) << "Created child span: "
                             << child_span.context.span.to_hex();
}

TEST_CASE("telemetry_context_start_linked_trace_creates_new_trace", tags) {
    auto lg(make_logger(test_suite));

    auto res = create_test_resource();
    auto parent_ctx = telemetry_context::create_root(res);

    auto [linked_ctx, linked_span] =
        parent_ctx.start_linked_trace("grid_computation");

    // Linked context should have DIFFERENT trace_id
    REQUIRE(linked_ctx.get_trace_id() != parent_ctx.get_trace_id());

    // Linked span should NOT have a parent (it's a root span)
    REQUIRE_FALSE(linked_span.parent_span_id.has_value());

    // Linked span should have a link back to the original
    REQUIRE(linked_span.links.size() == 1);
    REQUIRE(linked_span.links[0].context.trace == parent_ctx.get_trace_id());
    REQUIRE(linked_span.links[0].context.span == parent_ctx.get_span_id());

    // Link should have relationship attribute
    auto it = linked_span.links[0].attrs.find(
        std::string(semconv::link::relationship));
    REQUIRE(it != linked_span.links[0].attrs.end());
    REQUIRE(std::get<std::string>(it->second) ==
            semconv::link::triggered_by);

    BOOST_LOG_SEV(lg, debug) << "Created linked trace: "
                             << linked_span.context.trace.to_hex()
                             << " linked from: "
                             << parent_ctx.get_trace_id().to_hex();
}

TEST_CASE("telemetry_context_shares_resource", tags) {
    auto res = create_test_resource();
    auto parent_ctx = telemetry_context::create_root(res);

    auto [child_ctx, child_span] = parent_ctx.start_span("child_op");

    // Both contexts should share the same resource
    REQUIRE(parent_ctx.resource_ptr() == child_ctx.resource_ptr());
}

TEST_CASE("span_is_root_works_correctly", tags) {
    auto res = create_test_resource();
    auto parent_ctx = telemetry_context::create_root(res);

    // Create a root span (no parent)
    span root_span;
    root_span.context = parent_ctx.context();
    root_span.name = "root";
    REQUIRE(root_span.is_root());

    // Create a child span (has parent)
    auto [child_ctx, child_span] = parent_ctx.start_span("child");
    REQUIRE_FALSE(child_span.is_root());
}

TEST_CASE("span_duration_calculation", tags) {
    span s;
    s.start_time = std::chrono::system_clock::now();

    // Duration should be empty when span hasn't ended
    REQUIRE_FALSE(s.duration().has_value());

    // Set end time
    std::this_thread::sleep_for(std::chrono::milliseconds(10));
    s.end_time = std::chrono::system_clock::now();

    // Duration should now be calculable
    REQUIRE(s.duration().has_value());
    REQUIRE(s.duration().value().count() >= 10'000'000); // At least 10ms in ns
}

TEST_CASE("span_is_session_checks_attribute", tags) {
    span s;
    REQUIRE_FALSE(s.is_session());

    s.attrs["session.id"] = std::string("test-session-123");
    REQUIRE(s.is_session());
}

TEST_CASE("resource_from_environment_populates_attributes", tags) {
    auto lg(make_logger(test_suite));

    auto res = resource::from_environment("my.service", "2.0.0");

    REQUIRE(res.service_name().has_value());
    REQUIRE(res.service_name().value() == "my.service");

    REQUIRE(res.host_name().has_value());
    REQUIRE_FALSE(res.host_name().value().empty());

    REQUIRE(res.host_id().has_value());
    REQUIRE_FALSE(res.host_id().value().empty());

    BOOST_LOG_SEV(lg, debug) << "Resource - service: "
                             << res.service_name().value()
                             << ", host: " << res.host_name().value()
                             << ", host_id: " << res.host_id().value();
}
