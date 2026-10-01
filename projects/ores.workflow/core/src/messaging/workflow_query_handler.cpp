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
#include "ores.workflow.core/messaging/workflow_query_handler.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include "ores.workflow.api/service/workflow_definition.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <rfl/json.hpp>
#include <exception>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

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

std::string workflow_query_handler::state_name(const boost::uuids::uuid& id) const {
    if (const auto it = instance_state_names_.find(id); it != instance_state_names_.end())
        return it->second;
    if (const auto it = step_state_names_.find(id); it != step_state_names_.end())
        return it->second;
    return "unknown";
}

namespace {

std::string fmt_tp(const std::chrono::system_clock::time_point& tp) {
    return ores::platform::time::datetime::to_iso8601_utc(tp);
}

std::optional<std::string>
fmt_opt_tp(const std::optional<std::chrono::system_clock::time_point>& opt) {
    if (!opt)
        return std::nullopt;
    return fmt_tp(*opt);
}

} // namespace

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

    auto req = decode<list_workflow_instance_summaries_request>(msg);
    if (!req) {
        reply(nats_,
              msg,
              list_workflow_instance_summaries_response{.success = false,
                                                        .message = "Invalid request payload."});
        return;
    }

    const int limit = std::clamp(req->limit, 1, 1000);

    // read_latest uses ctx.tenant_id() for RLS filtering.
    auto instances = instance_repo_.read_latest(req_ctx);

    // Every filter is applied here rather than in the repository, so the read
    // stays one method. The rows in flight bound the work, and a filter that
    // named a column the repository does not index as a set would cost more
    // there than the scan it saves here.
    if (!req->status_filter.empty()) {
        const std::string& filter = req->status_filter;
        std::erase_if(instances,
                      [&](const auto& inst) { return state_name(inst.state_id) != filter; });
    }
    if (!req->type_filter.empty()) {
        const std::string& filter = req->type_filter;
        std::erase_if(instances, [&](const auto& inst) { return inst.type != filter; });
    }
    if (!req->target_kind_filter.empty()) {
        const std::string& filter = req->target_kind_filter;
        std::erase_if(instances, [&](const auto& inst) { return inst.target_kind != filter; });
    }
    if (!req->target_id_filter.empty()) {
        const std::string& filter = req->target_id_filter;
        std::erase_if(instances, [&](const auto& inst) {
            // A run with no target does not act on the one that was asked for.
            return inst.target_id == boost::uuids::uuid{} ||
                   boost::uuids::to_string(inst.target_id) != filter;
        });
    }

    // Sort by the audit timestamp descending (most recent first).
    std::sort(instances.begin(), instances.end(), [](const auto& a, const auto& b) {
        return a.recorded_at > b.recorded_at;
    });

    // Trim to limit.
    if (static_cast<int>(instances.size()) > limit)
        instances.resize(static_cast<std::size_t>(limit));

    list_workflow_instance_summaries_response resp;
    resp.success = true;
    resp.instances.reserve(instances.size());

    for (const auto& inst : instances) {
        workflow_instance_summary s;
        s.id = boost::uuids::to_string(inst.id);
        s.type = inst.type;
        s.status = state_name(inst.state_id);
        s.current_step_index = inst.current_step_index;
        s.step_count = inst.step_count;
        s.correlation_id = inst.correlation_id;
        s.created_by = inst.created_by;
        s.created_at = fmt_tp(inst.recorded_at);
        s.completed_at = fmt_opt_tp(inst.completed_at);
        s.error = inst.error;
        s.target_kind = inst.target_kind;
        s.target_id = inst.target_id == boost::uuids::uuid{} ?
                          std::string{} :
                          boost::uuids::to_string(inst.target_id);
        resp.instances.push_back(std::move(s));
    }

    BOOST_LOG_SEV(lg(), debug) << "list_instances returning " << resp.instances.size()
                               << " instance(s)";
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
        reply(nats_,
              msg,
              get_workflow_steps_response{.success = false, .message = "Invalid request payload."});
        return;
    }

    if (req->workflow_instance_id.empty()) {
        reply(nats_,
              msg,
              get_workflow_steps_response{.success = false,
                                          .message = "workflow_instance_id is required."});
        return;
    }

    // Parse and validate the instance UUID.
    boost::uuids::uuid instance_id;
    try {
        instance_id = boost::lexical_cast<boost::uuids::uuid>(req->workflow_instance_id);
    } catch (...) {
        reply(nats_,
              msg,
              get_workflow_steps_response{.success = false,
                                          .message = "Invalid workflow_instance_id."});
        return;
    }

    // The instance is read through the service's own context, which reaches
    // every tenant, and the guard below is what confines the answer to the
    // caller's. The steps are then read through the caller's context, so they
    // need no guard of their own.
    const auto instances = instance_repo_.read_latest(ctx_, boost::uuids::to_string(instance_id));
    if (instances.empty()) {
        reply(nats_,
              msg,
              get_workflow_steps_response{.success = false,
                                          .message = "Workflow instance not found."});
        return;
    }
    const auto* instance = &instances.front();

    // Tenant isolation guard: verify the instance belongs to the caller.
    if (instance->tenant_id != req_ctx.tenant_id()) {
        reply(nats_,
              msg,
              get_workflow_steps_response{.success = false,
                                          .message = "Workflow instance not found."});
        return;
    }

    // Load the steps, ordered by step_index ascending by the repository. A
    // workflow's step count is bounded by its definition, so one page covers it.
    const auto raw_steps = step_repo_.read_latest_by_workflow_id(
        req_ctx, boost::uuids::to_string(instance_id), 0, 1000);

    /*
     * The run's own definition travels with it, materialised when it started,
     * so the words a step is shown by are the ones its definition declared
     * rather than whatever the definition says today. An instance started
     * before a step had words carries none, and the step's identity stands in.
     */
    std::unordered_map<std::string, std::pair<std::string, std::string>> words;
    if (!instance->materialised_steps_json.empty()) {
        auto materialised =
            rfl::json::read<std::vector<ores::workflow::service::materialised_step>>(
                instance->materialised_steps_json);
        if (materialised) {
            for (const auto& step : *materialised)
                words.emplace(step.name, std::make_pair(step.label, step.description));
        }
    }

    get_workflow_steps_response resp;
    resp.success = true;
    resp.status = state_name(instance->state_id);
    resp.error = instance->error;
    resp.step_count = instance->step_count;
    resp.current_step_index = instance->current_step_index;
    resp.steps.reserve(raw_steps.size());

    for (const auto& s : raw_steps) {
        // Skip compensation steps (negative step_index) for Phase 1.
        if (s.step_index < 0)
            continue;

        workflow_step_summary ws;
        ws.id = boost::uuids::to_string(s.id);
        ws.name = s.name;
        if (const auto found = words.find(s.name); found != words.end()) {
            ws.label = found->second.first;
            ws.description = found->second.second;
        }
        ws.status = state_name(s.state_id);
        ws.step_index = s.step_index;
        ws.created_at = fmt_tp(s.recorded_at);
        ws.started_at = fmt_opt_tp(s.started_at);
        ws.completed_at = fmt_opt_tp(s.completed_at);
        ws.error = s.error;
        if (!s.step_log_json.empty()) {
            auto log = rfl::json::read<std::vector<ores::workflow::messaging::step_log_entry>>(
                s.step_log_json);
            if (log)
                ws.log = std::move(*log);
        }
        resp.steps.push_back(std::move(ws));
    }

    BOOST_LOG_SEV(lg(), debug) << "get_steps returning " << resp.steps.size() << " step(s)"
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
            ds.type_name = def.type_name;
            ds.description = def.description;

            // A definition whose steps do not depend on its request yields its
            // canonical sequence from an empty one. A definition whose steps the
            // request decides -- tenant provisioning builds one step per kind its
            // profile orders -- refuses an empty request, and is listed with no
            // steps rather than failing the whole read.
            std::vector<service::workflow_step_def> steps;
            try {
                steps = def.build_steps("", "", "");
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), debug) << "Definition " << def.type_name
                                           << " builds its steps from its request: " << e.what();
            }
            ds.step_count = static_cast<int>(steps.size());

            for (int i = 0; i < static_cast<int>(steps.size()); ++i) {
                const auto& s = steps[static_cast<std::size_t>(i)];
                workflow_step_definition_summary ss;
                ss.step_index = i;
                ss.name = s.name;
                ss.description = s.description;
                ss.command_subject = s.command_subject;
                ss.has_compensation = !s.compensation_subject.empty();
                ds.steps.push_back(std::move(ss));
            }

            resp.definitions.push_back(std::move(ds));
        }
    }

    // Sort by type_name for stable ordering.
    std::sort(resp.definitions.begin(), resp.definitions.end(), [](const auto& a, const auto& b) {
        return a.type_name < b.type_name;
    });

    BOOST_LOG_SEV(lg(), debug) << "list_definitions returning " << resp.definitions.size()
                               << " definition(s)";
    reply(nats_, msg, resp);
}

void workflow_query_handler::get_step_result(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "get_step_result request received";

    auto req = decode<get_step_result_request>(msg);
    if (!req || req->step_id.empty() || req->tenant_id.empty()) {
        reply(nats_, msg, get_step_result_response{.found = false});
        return;
    }

    boost::uuids::uuid step_uuid;
    try {
        step_uuid = boost::lexical_cast<boost::uuids::uuid>(req->step_id);
    } catch (...) {
        reply(nats_, msg, get_step_result_response{.found = false});
        return;
    }

    // The caller names the tenant the step belongs to, and the read is scoped to
    // it. This path serves a workflow command's idempotency question -- the
    // engine gave the caller that tenant in the command's X-Tenant-Id header --
    // so the answer must stay inside it: the service's own context would reach
    // every tenant's steps, and this reply carries the step's result, error and
    // log.
    //
    // The id is parsed rather than resolved. There is no code to look up, because
    // the engine minted the value it put in the header, and a request naming a
    // tenant that does not exist can only be asking for rows the policy will not
    // show it, so the read below answers found = false without an extra query.
    // This runs on every step dispatch, so it stays off the database.
    const auto tenant = ores::utility::uuid::tenant_id::from_string(req->tenant_id);
    if (!tenant) {
        BOOST_LOG_SEV(lg(), warn) << "get_step_result names tenant '" << req->tenant_id
                                  << "', which is not a tenant id.";
        reply(nats_, msg, get_step_result_response{.found = false});
        return;
    }
    const auto req_ctx = ctx_.with_tenant(*tenant, ctx_.actor());

    const auto steps = step_repo_.read_latest(req_ctx, boost::uuids::to_string(step_uuid));
    if (steps.empty()) {
        reply(nats_, msg, get_step_result_response{.found = false});
        return;
    }
    const auto* step = &steps.front();

    if (step->tenant_id != req_ctx.tenant_id()) {
        reply(nats_, msg, get_step_result_response{.found = false});
        return;
    }

    // Return cached result only for terminal states; in_progress means the
    // previous execution is still in flight (or was interrupted mid-publish).
    const auto sname = state_name(step->state_id);
    if (sname == "in_progress" || sname == "pending" || sname == "unknown") {
        reply(nats_, msg, get_step_result_response{.found = false});
        return;
    }

    using outcome = ores::workflow::messaging::step_outcome;
    const auto step_outcome = (sname == "completed") ? outcome::completed :
                              (sname == "completed_with_warnings") ?
                                                       outcome::completed_with_warnings :
                                                       outcome::failed;
    const bool is_success =
        step_outcome == outcome::completed || step_outcome == outcome::completed_with_warnings;

    BOOST_LOG_SEV(lg(), debug) << "get_step_result: step=" << req->step_id << " state=" << sname;

    std::vector<ores::workflow::messaging::step_log_entry> log;
    if (!step->step_log_json.empty()) {
        auto parsed = rfl::json::read<std::vector<ores::workflow::messaging::step_log_entry>>(
            step->step_log_json);
        if (parsed)
            log = std::move(*parsed);
    }

    reply(nats_,
          msg,
          get_step_result_response{.found = true,
                                   .outcome = step_outcome,
                                   .result_json = is_success ? step->response_json : "",
                                   .error_message = is_success ? "" : step->error,
                                   .log = std::move(log),
                                   .success = true});
}

} // namespace ores::workflow::messaging
