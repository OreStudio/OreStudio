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
#include "ores.workflow/service/run_report_workflow.hpp"

#include <chrono>
#include <format>
#include <string_view>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.workflow/domain/workflow_step.hpp"
#include "ores.workflow/service/fsm_state_map.hpp"
#include "ores.workflow/repository/workflow_step_repository.hpp"

namespace ores::workflow::service {

using namespace ores::logging;

namespace {

inline static std::string_view logger_name =
    "ores.workflow.service.run_report_workflow";

static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

/**
 * @brief Makes an authenticated NATS request and deserializes the response.
 */
template<typename Req>
std::optional<typename Req::response_type>
nats_call(ores::nats::service::nats_client& nats, const Req& request,
    std::string& out_error) {
    using Resp = typename Req::response_type;
    try {
        const auto json = rfl::json::write(request);
        const auto msg = nats.authenticated_request(Req::nats_subject, json);

        const auto err_it = msg.headers.find(
            std::string(ores::nats::headers::x_error));
        if (err_it != msg.headers.end()) {
            out_error = std::format("Service error on {}: {}",
                Req::nats_subject, err_it->second);
            return std::nullopt;
        }
        const std::string_view sv(
            reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
        auto result = rfl::json::read<Resp>(sv);
        if (!result) {
            out_error = std::format("Failed to parse response from {}: {}",
                Req::nats_subject, result.error().what());
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        out_error = std::format("Exception calling {}: {}",
            Req::nats_subject, e.what());
        return std::nullopt;
    }
}

domain::workflow_step make_step(const boost::uuids::uuid& workflow_id,
    int index, std::string_view name, std::string_view request_json,
    const boost::uuids::uuid& in_progress_state_id) {
    domain::workflow_step s;
    s.id = boost::uuids::random_generator()();
    s.workflow_id = workflow_id;
    s.step_index = index;
    s.name = std::string(name);
    s.state_id = in_progress_state_id;
    s.request_json = std::string(request_json);
    s.started_at = std::chrono::system_clock::now();
    s.created_at = *s.started_at;
    return s;
}

} // namespace

run_report_workflow::run_report_workflow(boost::uuids::uuid workflow_id,
    std::string report_instance_id,
    std::string tenant_id,
    std::string definition_id)
    : workflow_id_(workflow_id)
    , report_instance_id_(std::move(report_instance_id))
    , tenant_id_(std::move(tenant_id))
    , definition_id_(std::move(definition_id)) {}

bool run_report_workflow::execute(
    ores::database::context ctx,
    ores::nats::service::nats_client& nats) {
    BOOST_LOG_SEV(lg(), debug) << "Executing run_report_workflow for instance "
                               << report_instance_id_;

    const auto step_states = load_fsm_states(nats, "workflow_step");
    const auto step_in_progress = step_states.require("in_progress");
    const auto step_completed = step_states.require("completed");
    const auto step_failed = step_states.require("failed");

    repository::workflow_step_repository step_repo;

    // ----------------------------------------------------------------
    // Step 0: mark report instance as running
    // ----------------------------------------------------------------
    ores::reporting::messaging::mark_report_instance_running_request mark_running{
        .instance_id = report_instance_id_,
        .tenant_id = tenant_id_};
    const auto mark_running_json = rfl::json::write(mark_running);

    auto step0 = make_step(workflow_id_, 0, "mark_running",
        mark_running_json, step_in_progress);
    step_repo.create(ctx, step0);

    auto running_resp = nats_call(nats, mark_running, error_);
    if (!running_resp || !running_resp->success) {
        if (error_.empty())
            error_ = std::format("mark_running failed for instance {}: {}",
                report_instance_id_,
                running_resp ? running_resp->message : "(no response)");
        step_repo.update_state(ctx, step0.id, step_failed, "", error_);
        return false;
    }
    step_repo.update_state(ctx, step0.id, step_completed,
        rfl::json::write(*running_resp), "");
    completed_steps_ = 1;

    // ----------------------------------------------------------------
    // Step 1: ORE execution (stub)
    // ----------------------------------------------------------------
    BOOST_LOG_SEV(lg(), info) << "run_report_workflow: ORE execution stub — "
                              << "definition=" << definition_id_
                              << " instance=" << report_instance_id_
                              << " (not yet implemented)";

    auto step1 = make_step(workflow_id_, 1, "ore_execution",
        "{}", step_in_progress);
    step_repo.create(ctx, step1);
    // Stub: always succeeds immediately
    step_repo.update_state(ctx, step1.id, step_completed, "{}", "");
    completed_steps_ = 2;

    // ----------------------------------------------------------------
    // Step 2: mark report instance as completed
    // ----------------------------------------------------------------
    ores::reporting::messaging::mark_report_instance_completed_request mark_completed{
        .instance_id = report_instance_id_,
        .tenant_id = tenant_id_,
        .output_message = "Completed by run_report_workflow (ORE stub)"};
    const auto mark_completed_json = rfl::json::write(mark_completed);

    auto step2 = make_step(workflow_id_, 2, "mark_completed",
        mark_completed_json, step_in_progress);
    step_repo.create(ctx, step2);

    auto completed_resp = nats_call(nats, mark_completed, error_);
    if (!completed_resp || !completed_resp->success) {
        if (error_.empty())
            error_ = std::format("mark_completed failed for instance {}: {}",
                report_instance_id_,
                completed_resp ? completed_resp->message : "(no response)");
        step_repo.update_state(ctx, step2.id, step_failed, "", error_);
        return false;
    }
    step_repo.update_state(ctx, step2.id, step_completed,
        rfl::json::write(*completed_resp), "");
    completed_steps_ = 3;

    BOOST_LOG_SEV(lg(), info) << "run_report_workflow completed for instance "
                              << report_instance_id_;
    return true;
}

void run_report_workflow::compensate(
    ores::database::context ctx,
    ores::nats::service::nats_client& nats) {
    BOOST_LOG_SEV(lg(), info) << "Compensating run_report_workflow for instance "
                              << report_instance_id_;

    if (completed_steps_ >= 1) {
        ores::reporting::messaging::mark_report_instance_failed_request mark_failed{
            .instance_id = report_instance_id_,
            .tenant_id = tenant_id_,
            .error_message = error_};
        std::string err;
        auto r = nats_call(nats, mark_failed, err);
        if (!r || !r->success) {
            const auto reason = (r && !r->message.empty()) ? r->message : err;
            BOOST_LOG_SEV(lg(), error)
                << "Compensation mark_failed failed for instance "
                << report_instance_id_ << ": " << reason;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "run_report_workflow compensation complete.";
    (void)ctx;
}

}
