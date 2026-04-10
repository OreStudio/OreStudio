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
#include "ores.workflow/messaging/workflow_query_handler.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"

namespace ores::workflow::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

workflow_query_handler::workflow_query_handler(
    ores::nats::service::client& nats,
    ores::database::context ctx,
    ores::security::jwt::jwt_authenticator signer,
    const service::fsm_state_map& instance_states,
    const service::fsm_state_map& step_states,
    std::shared_ptr<const service::workflow_registry> registry)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , signer_(std::move(signer))
    , registry_(std::move(registry)) {

    // Build reverse maps: UUID → name
    for (const auto& [name, uuid] : instance_states.states)
        instance_state_names_[uuid] = name;
    for (const auto& [name, uuid] : step_states.states)
        step_state_names_[uuid] = name;
}

std::string workflow_query_handler::state_name(
    const boost::uuids::uuid& id) const {
    if (const auto it = instance_state_names_.find(id);
        it != instance_state_names_.end())
        return it->second;
    if (const auto it = step_state_names_.find(id);
        it != step_state_names_.end())
        return it->second;
    return "unknown";
}

namespace {

std::string fmt_tp(const std::chrono::system_clock::time_point& tp) {
    return ores::platform::time::datetime::to_iso8601_utc(tp);
}

std::optional<std::string> fmt_opt_tp(
    const std::optional<std::chrono::system_clock::time_point>& opt) {
    if (!opt) return std::nullopt;
    return fmt_tp(*opt);
}

}  // namespace

void workflow_query_handler::list_instances(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "list_instances request received";

    // Validate JWT and build tenant-scoped context.
    auto ctx_expected = ores::service::service::make_request_context(
        ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>(signer_));
    if (!ctx_expected) {
        error_reply(nats_, msg, ctx_expected.error());
        return;
    }
    const auto& req_ctx = *ctx_expected;

    auto req = decode<list_workflow_instances_request>(msg);
    if (!req) {
        reply(nats_, msg, list_workflow_instances_response{
            .success = false, .message = "Invalid request payload."});
        return;
    }

    const int limit = std::clamp(req->limit, 1, 1000);

    // read() uses ctx.tenant_id() for RLS filtering.
    auto instances = instance_repo_.read(req_ctx);

    // Apply optional status filter client-side (avoids a custom repo method).
    if (req->status_filter && !req->status_filter->empty()) {
        const std::string& filter = *req->status_filter;
        std::erase_if(instances, [&](const auto& inst) {
            return state_name(inst.state_id) != filter;
        });
    }

    // Sort by created_at descending (most recent first).
    std::sort(instances.begin(), instances.end(),
        [](const auto& a, const auto& b) {
            return a.created_at > b.created_at;
        });

    // Trim to limit.
    if (static_cast<int>(instances.size()) > limit)
        instances.resize(static_cast<std::size_t>(limit));

    list_workflow_instances_response resp;
    resp.success = true;
    resp.instances.reserve(instances.size());

    for (const auto& inst : instances) {
        workflow_instance_summary s;
        s.id                 = boost::uuids::to_string(inst.id);
        s.type               = inst.type;
        s.status             = state_name(inst.state_id);
        s.current_step_index = inst.current_step_index;
        s.step_count         = inst.step_count;
        s.correlation_id     = inst.correlation_id;
        s.created_by         = inst.created_by;
        s.created_at         = fmt_tp(inst.created_at);
        s.completed_at       = fmt_opt_tp(inst.completed_at);
        s.error              = inst.error;
        resp.instances.push_back(std::move(s));
    }

    BOOST_LOG_SEV(lg(), debug)
        << "list_instances returning " << resp.instances.size() << " instance(s)";
    reply(nats_, msg, resp);
}

void workflow_query_handler::get_steps(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "get_steps request received";

    // Validate JWT and build tenant-scoped context.
    auto ctx_expected = ores::service::service::make_request_context(
        ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>(signer_));
    if (!ctx_expected) {
        error_reply(nats_, msg, ctx_expected.error());
        return;
    }
    const auto& req_ctx = *ctx_expected;

    auto req = decode<get_workflow_steps_request>(msg);
    if (!req) {
        reply(nats_, msg, get_workflow_steps_response{
            .success = false, .message = "Invalid request payload."});
        return;
    }

    if (req->workflow_instance_id.empty()) {
        reply(nats_, msg, get_workflow_steps_response{
            .success = false, .message = "workflow_instance_id is required."});
        return;
    }

    // Parse and validate the instance UUID.
    boost::uuids::uuid instance_id;
    try {
        instance_id = boost::lexical_cast<boost::uuids::uuid>(
            req->workflow_instance_id);
    } catch (...) {
        reply(nats_, msg, get_workflow_steps_response{
            .success = false, .message = "Invalid workflow_instance_id."});
        return;
    }

    // Load the instance (service-account context — no tenant filter).
    auto instance = instance_repo_.find_by_id(ctx_, instance_id);
    if (!instance) {
        reply(nats_, msg, get_workflow_steps_response{
            .success = false, .message = "Workflow instance not found."});
        return;
    }

    // Tenant isolation guard: verify the instance belongs to the caller.
    if (instance->tenant_id != req_ctx.tenant_id().to_uuid()) {
        reply(nats_, msg, get_workflow_steps_response{
            .success = false, .message = "Workflow instance not found."});
        return;
    }

    // Load all steps (ordered by step_index ascending by the repository).
    const auto raw_steps = step_repo_.find_by_workflow_id(ctx_, instance_id);

    get_workflow_steps_response resp;
    resp.success = true;
    resp.steps.reserve(raw_steps.size());

    for (const auto& s : raw_steps) {
        // Skip compensation steps (negative step_index) for Phase 1.
        if (s.step_index < 0)
            continue;

        workflow_step_summary ws;
        ws.id         = boost::uuids::to_string(s.id);
        ws.name       = s.name;
        ws.status     = state_name(s.state_id);
        ws.step_index = s.step_index;
        ws.created_at = fmt_tp(s.created_at);
        ws.started_at = fmt_opt_tp(s.started_at);
        ws.completed_at = fmt_opt_tp(s.completed_at);
        ws.error      = s.error;
        resp.steps.push_back(std::move(ws));
    }

    BOOST_LOG_SEV(lg(), debug)
        << "get_steps returning " << resp.steps.size() << " step(s)"
        << " for workflow=" << req->workflow_instance_id;
    reply(nats_, msg, resp);
}

void workflow_query_handler::list_definitions(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "list_definitions request received";

    list_workflow_definitions_response resp;
    resp.success = true;

    if (registry_) {
        for (const auto& [type_name, def] : registry_->all()) {
            workflow_definition_summary ds;
            ds.type_name   = def.type_name;
            ds.description = def.description;
            ds.step_count  = static_cast<int>(def.steps.size());

            for (int i = 0; i < static_cast<int>(def.steps.size()); ++i) {
                const auto& s = def.steps[static_cast<std::size_t>(i)];
                workflow_step_definition_summary ss;
                ss.step_index       = i;
                ss.name             = s.name;
                ss.description      = s.description;
                ss.command_subject   = s.command_subject;
                ss.has_compensation  = !s.compensation_subject.empty();
                ds.steps.push_back(std::move(ss));
            }

            resp.definitions.push_back(std::move(ds));
        }
    }

    // Sort by type_name for stable ordering.
    std::sort(resp.definitions.begin(), resp.definitions.end(),
        [](const auto& a, const auto& b) {
            return a.type_name < b.type_name;
        });

    BOOST_LOG_SEV(lg(), debug)
        << "list_definitions returning " << resp.definitions.size()
        << " definition(s)";
    reply(nats_, msg, resp);
}

}  // namespace ores::workflow::messaging
