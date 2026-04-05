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
#include "ores.workflow/service/workflow_engine.hpp"

#include <chrono>
#include <span>
#include <cstddef>
#include <format>
#include <ranges>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow/messaging/workflow_events.hpp"

namespace ores::workflow::service {

using namespace ores::logging;

workflow_engine::workflow_engine(ores::nats::service::client& nats,
    ores::database::context ctx,
    const workflow_registry& registry,
    fsm_state_map instance_states,
    fsm_state_map step_states)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , registry_(registry)
    , instance_states_(std::move(instance_states))
    , step_states_(std::move(step_states)) {}

void workflow_engine::publish_command(
    const domain::workflow_step& step,
    const boost::uuids::uuid& instance_id) {

    const auto step_id_str = boost::uuids::to_string(step.id);
    const auto inst_id_str = boost::uuids::to_string(instance_id);

    const auto data = std::as_bytes(
        std::span{step.command_json.data(), step.command_json.size()});

    nats_.publish(step.command_subject, data, {
        {"X-Workflow-Step-Id",     step_id_str},
        {"X-Workflow-Instance-Id", inst_id_str}
    });
}

void workflow_engine::dispatch_next_step(
    domain::workflow_instance& instance,
    const std::string& last_result_json) {

    const auto* def = registry_.find(instance.type);
    if (!def) {
        BOOST_LOG_SEV(lg(), error)
            << "No workflow definition for type: " << instance.type;
        instance_repo_.update_state(ctx_, instance.id,
            instance_states_.require("failed"), "",
            "Unknown workflow type: " + instance.type);
        return;
    }

    const int next_index = instance.current_step_index + 1;

    if (next_index >= instance.step_count) {
        // All steps complete.
        BOOST_LOG_SEV(lg(), info)
            << "Workflow " << boost::uuids::to_string(instance.id)
            << " completed all " << instance.step_count << " steps.";
        instance_repo_.update_state(ctx_, instance.id,
            instance_states_.require("completed"), last_result_json, "");
        return;
    }

    // Build the command for the next step.
    const auto& step_def = def->steps[next_index];
    const auto all_steps = step_repo_.find_by_workflow_id(ctx_, instance.id);

    std::vector<std::string> results;
    results.reserve(all_steps.size());
    for (const auto& s : all_steps) {
        if (!s.response_json.empty())
            results.push_back(s.response_json);
    }

    const auto cmd_json = step_def.build_command(instance.request_json, results);
    const auto next_id = boost::uuids::random_generator()();

    domain::workflow_step next_step;
    next_step.id = next_id;
    next_step.workflow_id = instance.id;
    next_step.step_index = next_index;
    next_step.name = step_def.name;
    next_step.state_id = step_states_.require("in_progress");
    next_step.request_json = cmd_json;
    next_step.command_subject = step_def.command_subject;
    next_step.command_json = cmd_json;
    next_step.idempotency_key = boost::uuids::to_string(next_id);
    next_step.compensation_subject = step_def.compensation_subject;
    next_step.created_at = std::chrono::system_clock::now();

    // Persist before publishing (ensures restart can re-dispatch).
    step_repo_.create(ctx_, next_step);

    // Publish the command.
    publish_command(next_step, instance.id);

    // Record that the command was published.
    step_repo_.mark_command_published(ctx_, next_id);

    // Advance the instance's step index.
    instance_repo_.update_step_progress(ctx_, instance.id, next_index);
    instance.current_step_index = next_index;

    BOOST_LOG_SEV(lg(), info)
        << "Dispatched step " << next_index << " ("
        << step_def.name << ") for workflow "
        << boost::uuids::to_string(instance.id);
}

void workflow_engine::begin_compensation(
    const domain::workflow_instance& instance,
    const std::string& failure_msg) {

    BOOST_LOG_SEV(lg(), warn)
        << "Beginning compensation for workflow "
        << boost::uuids::to_string(instance.id)
        << ": " << failure_msg;

    instance_repo_.update_state(ctx_, instance.id,
        instance_states_.require("compensating"), "", failure_msg);

    const auto* def = registry_.find(instance.type);
    if (!def) {
        BOOST_LOG_SEV(lg(), error)
            << "Cannot compensate: no definition for type " << instance.type;
        instance_repo_.update_state(ctx_, instance.id,
            instance_states_.require("compensated"), "", failure_msg);
        return;
    }

    // Load completed steps in reverse order for compensation.
    auto steps = step_repo_.find_by_workflow_id(ctx_, instance.id);
    std::ranges::reverse(steps);

    for (const auto& s : steps) {
        // Only compensate successfully completed steps.
        if (s.state_id != step_states_.require("completed"))
            continue;

        const auto step_index = static_cast<std::size_t>(s.step_index);
        if (step_index >= def->steps.size())
            continue;

        const auto& step_def = def->steps[step_index];
        if (step_def.compensation_subject.empty())
            continue;

        const auto comp_json = step_def.build_compensation
            ? step_def.build_compensation(s.command_json, s.response_json)
            : "{}";

        // Persist compensation step (step_index negative to distinguish).
        const auto comp_id = boost::uuids::random_generator()();
        domain::workflow_step comp_step;
        comp_step.id = comp_id;
        comp_step.workflow_id = instance.id;
        comp_step.step_index = -(s.step_index + 1);
        comp_step.name = step_def.name + "_compensation";
        comp_step.state_id = step_states_.require("in_progress");
        comp_step.request_json = comp_json;
        comp_step.command_subject = step_def.compensation_subject;
        comp_step.command_json = comp_json;
        comp_step.idempotency_key = boost::uuids::to_string(comp_id);
        comp_step.created_at = std::chrono::system_clock::now();
        step_repo_.create(ctx_, comp_step);

        // Publish compensation command (fire-and-forget; no step-completed
        // event expected from compensation in Phase 1 — see plan Phase 1.2).
        const auto data = std::as_bytes(
            std::span{comp_json.data(), comp_json.size()});
        nats_.publish(step_def.compensation_subject, data, {
            {"X-Workflow-Step-Id",     boost::uuids::to_string(comp_id)},
            {"X-Workflow-Instance-Id", boost::uuids::to_string(instance.id)}
        });

        step_repo_.mark_command_published(ctx_, comp_id);
    }

    instance_repo_.update_state(ctx_, instance.id,
        instance_states_.require("compensated"), "", failure_msg);
}

void workflow_engine::on_step_completed(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "Handling step-completed event";

    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto event_result = rfl::json::read<messaging::step_completed_event>(sv);
    if (!event_result) {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to decode step_completed_event: "
            << event_result.error().what();
        return;
    }
    const auto& event = *event_result;

    BOOST_LOG_SEV(lg(), info)
        << "step_completed: step=" << event.step_id
        << " instance=" << event.workflow_instance_id
        << " success=" << event.success;

    // Load the workflow step.
    boost::uuids::uuid step_id;
    try {
        step_id = boost::lexical_cast<boost::uuids::uuid>(event.step_id);
    } catch (...) {
        BOOST_LOG_SEV(lg(), error) << "Invalid step_id: " << event.step_id;
        return;
    }

    auto step = step_repo_.find_by_id(ctx_, step_id);
    if (!step) {
        BOOST_LOG_SEV(lg(), warn) << "Step not found: " << event.step_id;
        return;
    }

    // Guard: duplicate event if step is already out of in_progress.
    if (step->state_id != step_states_.require("in_progress")) {
        BOOST_LOG_SEV(lg(), info)
            << "Duplicate step-completed event for step " << event.step_id
            << " (state is not in_progress); ignoring.";
        return;
    }

    // Update step state.
    if (event.success) {
        step_repo_.update_state(ctx_, step_id,
            step_states_.require("completed"), event.result_json, "");
    } else {
        step_repo_.update_state(ctx_, step_id,
            step_states_.require("failed"), "", event.error_message);
    }

    // Load the workflow instance.
    boost::uuids::uuid instance_id;
    try {
        instance_id = boost::lexical_cast<boost::uuids::uuid>(
            event.workflow_instance_id);
    } catch (...) {
        BOOST_LOG_SEV(lg(), error)
            << "Invalid workflow_instance_id: " << event.workflow_instance_id;
        return;
    }

    auto instance = instance_repo_.find_by_id(ctx_, instance_id);
    if (!instance) {
        BOOST_LOG_SEV(lg(), error)
            << "Workflow instance not found: " << event.workflow_instance_id;
        return;
    }

    if (event.success) {
        dispatch_next_step(*instance, event.result_json);
    } else {
        begin_compensation(*instance, event.error_message);
    }
}

void workflow_engine::on_start_workflow(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "Handling start-workflow message";

    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto msg_result = rfl::json::read<messaging::start_workflow_message>(sv);
    if (!msg_result) {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to decode start_workflow_message: "
            << msg_result.error().what();
        return;
    }
    const auto& req = *msg_result;

    const auto* def = registry_.find(req.type);
    if (!def) {
        BOOST_LOG_SEV(lg(), error) << "No workflow definition for type: " << req.type;
        return;
    }
    if (def->steps.empty()) {
        BOOST_LOG_SEV(lg(), error) << "Workflow definition has no steps: " << req.type;
        return;
    }

    // Parse tenant_id.
    boost::uuids::uuid tenant_id;
    try {
        tenant_id = boost::lexical_cast<boost::uuids::uuid>(req.tenant_id);
    } catch (...) {
        BOOST_LOG_SEV(lg(), error) << "Invalid tenant_id: " << req.tenant_id;
        return;
    }

    // Create the workflow instance.
    const auto instance_id = boost::uuids::random_generator()();
    domain::workflow_instance instance;
    instance.id = instance_id;
    instance.tenant_id = tenant_id;
    instance.type = req.type;
    instance.state_id = instance_states_.require("in_progress");
    instance.request_json = req.request_json;
    instance.correlation_id = req.correlation_id;
    instance.created_by = ctx_.service_account();
    instance.current_step_index = 0;
    instance.step_count = static_cast<int>(def->steps.size());
    instance.created_at = std::chrono::system_clock::now();

    instance_repo_.create(ctx_, instance);

    // Build and dispatch step 0.
    const auto& step_def = def->steps[0];
    const auto cmd_json = step_def.build_command(req.request_json, {});
    const auto step_id = boost::uuids::random_generator()();

    domain::workflow_step step;
    step.id = step_id;
    step.workflow_id = instance_id;
    step.step_index = 0;
    step.name = step_def.name;
    step.state_id = step_states_.require("in_progress");
    step.request_json = cmd_json;
    step.command_subject = step_def.command_subject;
    step.command_json = cmd_json;
    step.idempotency_key = boost::uuids::to_string(step_id);
    step.compensation_subject = step_def.compensation_subject;
    step.created_at = std::chrono::system_clock::now();

    step_repo_.create(ctx_, step);
    publish_command(step, instance_id);
    step_repo_.mark_command_published(ctx_, step_id);

    BOOST_LOG_SEV(lg(), info)
        << "Started workflow " << req.type
        << " instance=" << boost::uuids::to_string(instance_id)
        << " step_count=" << def->steps.size();
}

void workflow_engine::recover_in_progress() {
    BOOST_LOG_SEV(lg(), info) << "Starting workflow recovery pass.";

    const auto in_progress_id = instance_states_.require("in_progress");
    const auto instances = instance_repo_.find_by_state(ctx_, in_progress_id);

    BOOST_LOG_SEV(lg(), info)
        << "Found " << instances.size() << " in-progress workflow instance(s).";

    for (const auto& instance : instances) {
        try {
            // Find the current in-progress step for this instance.
            const auto steps = step_repo_.find_by_workflow_id(ctx_, instance.id);
            const auto it = std::ranges::find_if(steps, [&](const auto& s) {
                return s.state_id == step_states_.require("in_progress");
            });

            if (it == steps.end()) {
                BOOST_LOG_SEV(lg(), warn)
                    << "In-progress instance " << boost::uuids::to_string(instance.id)
                    << " has no in-progress step; skipping.";
                continue;
            }

            BOOST_LOG_SEV(lg(), info)
                << "Re-dispatching step " << it->step_index
                << " (" << it->name << ") for instance "
                << boost::uuids::to_string(instance.id);

            // Re-publish the command with the same step_id (idempotency key).
            // Domain services will detect the duplicate and re-publish their
            // step-completed event without re-executing the operation.
            publish_command(*it, instance.id);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Recovery failed for instance "
                << boost::uuids::to_string(instance.id)
                << ": " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Workflow recovery pass complete.";
}

}
