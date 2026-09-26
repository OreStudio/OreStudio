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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.workflow.api/messaging/workflow_events.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[workflow_helpers]");

}

using namespace ores::service::messaging;
using ores::workflow::messaging::instance_id_header;
using ores::workflow::messaging::step_id_header;
using ores::workflow::messaging::tenant_id_header;

TEST_CASE("a message is a workflow command only with the step id header", tags) {
    ores::nats::message msg;
    REQUIRE_FALSE(is_workflow_command(msg));

    msg.headers[std::string(step_id_header)] = "1c9f0a1e-0000-0000-0000-000000000001";
    REQUIRE(is_workflow_command(msg));
}

TEST_CASE("extract_workflow_header returns the value or an empty string", tags) {
    ores::nats::message msg;
    REQUIRE(extract_workflow_header(msg, step_id_header).empty());

    msg.headers[std::string(step_id_header)] = "step-1";
    REQUIRE(extract_workflow_header(msg, step_id_header) == "step-1");
    REQUIRE(extract_workflow_header(msg, tenant_id_header).empty());
}

TEST_CASE("workflow step context reads the three headers off a command", tags) {
    ores::nats::service::client nats{ores::nats::config::nats_options{}};
    ores::nats::message msg;
    msg.headers[std::string(step_id_header)] = "step-1";
    msg.headers[std::string(instance_id_header)] = "instance-1";
    msg.headers[std::string(tenant_id_header)] = "tenant-1";

    const auto ctx = workflow_step_context::from_message(nats, msg);

    REQUIRE(ctx.has_value());
    REQUIRE(ctx->step_id == "step-1");
    REQUIRE(ctx->instance_id == "instance-1");
    REQUIRE(ctx->tenant_id == "tenant-1");
    REQUIRE(ctx->nats == &nats);
}

TEST_CASE("workflow step context is absent on a message that is not a workflow command", tags) {
    ores::nats::service::client nats{ores::nats::config::nats_options{}};
    ores::nats::message msg;
    msg.headers[std::string(instance_id_header)] = "instance-1";

    REQUIRE_FALSE(workflow_step_context::from_message(nats, msg).has_value());
}

TEST_CASE("a cached step result defaults to a completed outcome with no payload", tags) {
    const cached_step_result result;

    REQUIRE(result.outcome == ores::workflow::messaging::step_outcome::completed);
    REQUIRE(result.result_json.empty());
    REQUIRE(result.error_message.empty());
    REQUIRE(result.log.empty());
}
