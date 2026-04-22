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
#ifndef ORES_REPORTING_MESSAGING_REPORT_INSTANCE_HANDLER_HPP
#define ORES_REPORTING_MESSAGING_REPORT_INSTANCE_HANDLER_HPP

#include <chrono>
#include <optional>
#include <span>
#include <cstddef>
#include <rfl/json.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.api/messaging/report_execution_protocol.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.reporting.core/service/report_definition_service.hpp"
#include "ores.workflow.api/messaging/workflow_events.hpp"
#include "ores.workflow/service/fsm_state_map.hpp"

namespace ores::reporting::messaging {

namespace {
inline auto& report_instance_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.reporting.messaging.report_instance_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

class report_instance_handler {
public:
    report_instance_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier,
        ores::workflow::service::fsm_state_map instance_states)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)),
          instance_states_(std::move(instance_states)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(report_instance_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::report_instance_service svc(ctx);
        get_report_instances_response resp;
        try {
            resp.instances = svc.list_instances();
            resp.total_available_count =
                static_cast<int>(resp.instances.size());
        } catch (...) {}
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(report_instance_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(report_instance_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "reporting::report_instances:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        if (auto req = decode<save_report_instance_request>(msg)) {
            service::report_instance_service svc(ctx);
            try {
                stamp(req->instance, ctx);
                svc.save_instance(req->instance);
                reply(nats_, msg,
                    save_report_instance_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_report_instance_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_instance_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(report_instance_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(report_instance_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "reporting::report_instances:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        if (auto req = decode<delete_report_instance_request>(msg)) {
            service::report_instance_service svc(ctx);
            try {
                for (const auto& id : req->ids)
                    svc.remove_instance(id);
                reply(nats_, msg,
                    delete_report_instance_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, delete_report_instance_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_instance_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(report_instance_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(report_instance_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (auto req = decode<get_report_instance_history_request>(msg)) {
            service::report_instance_service svc(ctx);
            try {
                auto hist = svc.get_instance_history(req->id);
                reply(nats_, msg, get_report_instance_history_response{
                    .success = true,
                    .history = std::move(hist)});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_report_instance_history_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_instance_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(report_instance_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    /**
     * @brief Fire-and-forget handler for scheduler trigger messages.
     *
     * Receives a trigger_report_instance_message published by the scheduler
     * when a report job fires. Applies the definition's concurrency policy,
     * creates a report_instance with the correct FSM state, and dispatches
     * a start_workflow_message for pending instances.
     */
    void trigger(ores::nats::message msg) {
        BOOST_LOG_SEV(report_instance_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto trigger_msg = decode<trigger_report_instance_message>(msg);
        if (!trigger_msg) {
            BOOST_LOG_SEV(report_instance_handler_lg(), warn)
                << "Failed to decode trigger message on: " << msg.subject;
            return;
        }

        try {
            const auto tid_result = ores::utility::uuid::tenant_id::from_string(
                trigger_msg->tenant_id);
            if (!tid_result) {
                BOOST_LOG_SEV(report_instance_handler_lg(), error)
                    << "Invalid tenant_id in trigger message: "
                    << trigger_msg->tenant_id;
                return;
            }
            const auto tenant_ctx = ctx_.with_tenant(*tid_result,
                ctx_.service_account());

            service::report_definition_service def_svc(tenant_ctx);
            const auto def = def_svc.find_definition(
                trigger_msg->report_definition_id);
            if (!def) {
                BOOST_LOG_SEV(report_instance_handler_lg(), error)
                    << "Report definition not found: "
                    << trigger_msg->report_definition_id;
                return;
            }

            // ── Concurrency check ────────────────────────────────────
            service::report_instance_service inst_svc(tenant_ctx);
            const auto existing = inst_svc.list_instances();
            const auto pending_id = instance_states_.require("pending");
            const auto running_id = instance_states_.require("running");
            bool has_active = false;
            for (const auto& e : existing) {
                if (e.definition_id != def->id) continue;
                if (e.fsm_state_id == pending_id ||
                    e.fsm_state_id == running_id) {
                    has_active = true;
                    break;
                }
            }

            // ── Determine initial FSM state ──────────────────────────
            boost::uuids::uuid initial_state;
            bool should_dispatch = false;
            if (!has_active) {
                initial_state = pending_id;
                should_dispatch = true;
            } else if (def->concurrency_policy == "queue") {
                initial_state = instance_states_.require("queued");
            } else if (def->concurrency_policy == "skip") {
                initial_state = instance_states_.require("skipped");
            } else {
                // "fail" policy or unknown — mark as failed.
                initial_state = instance_states_.require("failed");
            }

            // ── Create instance ──────────────────────────────────────
            boost::uuids::random_generator rg;
            domain::report_instance inst;
            inst.id = rg();
            inst.tenant_id = def->tenant_id;
            inst.party_id = def->party_id;
            inst.definition_id = def->id;
            inst.name = def->name;
            inst.description = def->description;
            inst.fsm_state_id = initial_state;
            inst.trigger_run_id = trigger_msg->job_instance_id;
            inst.started_at = std::chrono::system_clock::now();
            inst.modified_by = ctx_.service_account();
            inst.performed_by = ctx_.service_account();
            inst.change_reason_code = "system.scheduler_trigger";
            inst.change_commentary = "Created by scheduler trigger";

            inst_svc.save_instance(inst);

            const auto inst_id_str = boost::uuids::to_string(inst.id);
            BOOST_LOG_SEV(report_instance_handler_lg(), info)
                << "Created report instance " << inst_id_str
                << " for definition " << def->id
                << " state=" << (should_dispatch ? "pending" :
                    (def->concurrency_policy == "queue" ? "queued" :
                     def->concurrency_policy))
                << " (job_instance_id=" << trigger_msg->job_instance_id << ")";

            // ── Dispatch workflow for pending instances ───────────────
            if (should_dispatch) {
                const auto wf_instance_id = boost::uuids::to_string(rg());

                report_execution_request exec_req{
                    .report_instance_id = inst_id_str,
                    .definition_id = trigger_msg->report_definition_id,
                    .tenant_id = trigger_msg->tenant_id,
                    .correlation_id = inst_id_str};

                ores::workflow::messaging::start_workflow_message swm{
                    .type           = "report_execution_workflow",
                    .tenant_id      = trigger_msg->tenant_id,
                    .request_json   = rfl::json::write(exec_req),
                    .correlation_id = inst_id_str,
                    .instance_id    = wf_instance_id};

                const auto swm_json = rfl::json::write(swm);
                const auto data = std::as_bytes(
                    std::span{swm_json.data(), swm_json.size()});
                nats_.js_publish(
                    ores::workflow::messaging::start_workflow_message::nats_subject,
                    data);

                BOOST_LOG_SEV(report_instance_handler_lg(), info)
                    << "Dispatched report_execution_workflow for instance "
                    << inst_id_str << " wf_instance=" << wf_instance_id;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(report_instance_handler_lg(), error)
                << "Failed to create report instance for trigger: " << e.what();
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    ores::workflow::service::fsm_state_map instance_states_;
};

} // namespace ores::reporting::messaging

#endif
