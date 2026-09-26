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
#include "ores.workflow.core/messaging/identity_step_handler.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/workflow/identity_workflow.hpp"
#include <chrono>
#include <rfl/json.hpp>
#include <string_view>
#include <thread>

namespace identity_ns = ores::workflow::workflow;

namespace ores::workflow::messaging {

namespace {

using namespace ores::logging;
inline std::string_view logger_name = "ores.workflow.messaging.identity_step_handler";
auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

/**
 * @brief Answers one identity command with the outcome its payload asks for.
 *
 * The context is taken from the message headers rather than the body, exactly as
 * a real step handler takes it, so the fixture exercises the same contract.
 */
void handle(ores::nats::service::client& nats, ores::nats::message msg) {
    const auto ctx = ores::service::messaging::workflow_step_context::from_message(nats, msg);
    if (!ctx) {
        BOOST_LOG_SEV(lg(), warn) << "Identity command arrived without workflow headers on "
                                  << msg.subject;
        return;
    }

    // The engine publishes a step command as JSON whatever the process-wide wire
    // format is, so it is read as JSON here rather than through the codec, as
    // every other step handler reads it.
    const std::string_view payload(reinterpret_cast<const char*>(msg.data.data()),
                                   msg.data.size());
    const auto parsed = rfl::json::read<identity_ns::identity_step_request>(payload);
    if (!parsed) {
        ctx->fail("Failed to parse the identity step command: " +
                  std::string(parsed.error().what()));
        return;
    }

    const auto& step = *parsed;
    const auto outcome = step.outcome();
    const auto delay = step.delay();

    if (delay > 0) {
        BOOST_LOG_SEV(lg(), info) << "Identity step '" << step.name << "' waiting " << delay
                                  << "s so the run can be observed.";
        std::this_thread::sleep_for(std::chrono::seconds(delay));
    }

    const auto result_json = rfl::json::write(identity_ns::identity_step_request{
        .name = step.name, .behaviour = outcome, .compensate = false, .delay_seconds = 0});

    if (outcome == "warn") {
        // The log is the point of this path: a step can succeed and still have
        // skipped work, and only the log says so.
        BOOST_LOG_SEV(lg(), info) << "Identity step '" << step.name << "' reporting warnings.";
        ctx->warn(result_json,
                  {ores::workflow::messaging::step_log_entry{
                      .level = ores::workflow::messaging::step_log_level::warn,
                      .message = "identity step '" + step.name + "' deliberately warned",
                      .context = step.name}});
        return;
    }

    if (outcome == "fail") {
        BOOST_LOG_SEV(lg(), info) << "Identity step '" << step.name << "' reporting failure.";
        ctx->fail("identity step '" + step.name + "' deliberately failed",
                  {ores::workflow::messaging::step_log_entry{
                      .level = ores::workflow::messaging::step_log_level::error,
                      .message = "identity step '" + step.name + "' was asked to fail",
                      .context = step.name}});
        return;
    }

    if (outcome != "complete") {
        BOOST_LOG_SEV(lg(), warn) << "Identity step '" << step.name
                                  << "' asked for unknown behaviour '" << outcome << "'.";
        ctx->fail("identity step '" + step.name + "' asked for unknown behaviour '" + outcome +
                  "'");
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Identity step '" << step.name << "' reporting completion.";
    ctx->complete(result_json);
}

}

std::vector<ores::nats::service::subscription>
register_identity_step_handlers(ores::nats::service::client& nats, std::string_view queue_group) {
    std::vector<ores::nats::service::subscription> subs;

    subs.push_back(nats.queue_subscribe(
        std::string(identity_ns::identity_step_command_subject),
        std::string(queue_group),
        [&nats](ores::nats::message msg) { handle(nats, std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        std::string(identity_ns::identity_compensation_command_subject),
        std::string(queue_group),
        [&nats](ores::nats::message msg) { handle(nats, std::move(msg)); }));

    return subs;
}

}
