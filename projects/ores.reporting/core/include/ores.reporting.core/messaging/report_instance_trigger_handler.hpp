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
#ifndef ORES_REPORTING_CORE_MESSAGING_REPORT_INSTANCE_TRIGGER_HANDLER_HPP
#define ORES_REPORTING_CORE_MESSAGING_REPORT_INSTANCE_TRIGGER_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/service/report_definition_service.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.workflow.api/messaging/workflow_events.hpp"
#include "ores.workflow.core/service/fsm_state_map.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <optional>
#include <rfl/json.hpp>
#include <string>
#include <string_view>

namespace ores::reporting::messaging {

namespace {

// The concurrency policy codes the change-reason-free seed defines. Stated once
// so a reader sees the exact set this handler depends on; a fourth policy in the
// seed would have to be added here too, and the unknown-policy branch is what
// catches it if it is not.
constexpr std::string_view policy_fail = "fail";
constexpr std::string_view policy_queue = "queue";
constexpr std::string_view policy_skip = "skip";

inline auto& report_instance_trigger_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.reporting.messaging.report_instance_trigger_handler");
    return instance;
}
} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief Hand-crafted NATS handler for the report instance trigger operation.
 *
 * Lives outside the codegen-generated handlers because it is not entity CRUD:
 * it creates a report instance, applies the definition's concurrency policy,
 * and dispatches a workflow. It serves the operation subject
 * reporting.v1.ops.trigger_report_instance, which the scheduler publishes and
 * the shell can call by hand.
 *
 * The tenant comes from the request rather than from the caller's token,
 * because the scheduler runs as one service account in one tenant while
 * definitions belong to the tenants that scheduled them. The caller's
 * permission check is what decides whether it may act in that tenant.
 *
 * Every path answers. A trigger that cannot be honoured returns a failed
 * result naming why, so a scheduled run is never recorded as a success while
 * producing nothing.
 */
class report_instance_trigger_handler {
public:
    report_instance_trigger_handler(ores::nats::service::client& nats,
                                    ores::database::context ctx,
                                    std::optional<ores::security::jwt::jwt_authenticator> verifier,
                                    ores::workflow::service::fsm_state_map instance_states)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , instance_states_(std::move(instance_states)) {}

    void trigger(ores::nats::message msg) {
        BOOST_LOG_SEV(report_instance_trigger_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;

        auto req = decode<trigger_report_instance_request>(msg);
        if (!req) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        trigger_report_instance_response response;
        try {
            trigger_one(req_ctx, *req, response, msg);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(report_instance_trigger_handler_lg(), error)
                << "Trigger failed: " << e.what();
            response.result.outcome = ores::utility::domain::outcome::failed;
            response.result.code = "trigger_failed";
            response.result.message = e.what();
        }
        reply(nats_, msg, response);
    }

private:
    /**
     * @brief Runs one trigger, writing its outcome into @p response.
     *
     * The response's result starts as ok and is set to failed, with the reason,
     * wherever the trigger cannot proceed.
     */
    void trigger_one(const ores::database::context& req_ctx,
                     const trigger_report_instance_request& req,
                     trigger_report_instance_response& response,
                     const ores::nats::message& msg) {
        const auto tenant = boost::uuids::to_string(req.tenant_id);
        const auto definition_id = boost::uuids::to_string(req.report_definition_id);
        // Scoped from the authenticated context, not the base one: the
        // authenticated context carries the workspace the request resolved
        // to, and the generated reads filter on it.
        const auto tenant_ctx =
            ores::database::service::tenant_context::with_tenant(req_ctx, tenant);

        service::report_definition_service def_svc(tenant_ctx);
        const auto def = def_svc.get_definition(req.report_definition_id);
        if (!def) {
            response.result.outcome = ores::utility::domain::outcome::missing;
            response.result.code = "definition_not_found";
            response.result.message =
                std::format("No report definition {} in tenant {}.", definition_id, tenant);
            BOOST_LOG_SEV(report_instance_trigger_handler_lg(), warn) << response.result.message;
            return;
        }

        const auto policy = def->concurrency_policy;
        if (policy != policy_fail && policy != policy_queue && policy != policy_skip) {
            response.result.outcome = ores::utility::domain::outcome::invalid;
            response.result.code = "unknown_concurrency_policy";
            response.result.message = std::format(
                "Report definition {} states concurrency policy '{}', which is not one of "
                "fail, queue or skip.",
                definition_id,
                policy);
            BOOST_LOG_SEV(report_instance_trigger_handler_lg(), warn) << response.result.message;
            return;
        }

        const auto in_flight = find_in_flight(tenant, definition_id);

        boost::uuids::uuid initial_state = instance_states_.require("pending");
        bool dispatch = true;
        std::string note;
        if (in_flight) {
            note = std::format("An instance of this definition is already in flight ({}).",
                               *in_flight);
            if (policy == policy_queue) {
                initial_state = instance_states_.require("queued");
            } else if (policy == policy_skip) {
                initial_state = instance_states_.require("skipped");
            } else {
                initial_state = instance_states_.require("failed");
            }
            dispatch = false;
        }

        boost::uuids::random_generator rg;
        domain::report_instance inst;
        inst.id = rg();
        inst.tenant_id = def->tenant_id;
        inst.party_id = def->party_id;
        inst.definition_id = def->id;
        // An instance is one occurrence of the definition, and the natural key
        // is unique per tenant, so two runs of one definition cannot share a
        // name. The run's start is what distinguishes them.
        inst.name = std::format(
            "{} {}",
            def->name,
            ores::platform::time::datetime::to_db_string(std::chrono::system_clock::now()));
        inst.description = def->description;
        inst.fsm_state_id = initial_state;
        inst.trigger_run_id = req.job_instance_id;
        inst.output_message = note;
        inst.modified_by = ctx_.service_account();
        inst.performed_by = ctx_.service_account();
        // The seeded change reason the validator accepts for a system insert.
        inst.change_reason_code = "system.new_record";
        inst.change_commentary = note.empty() ? "Created by report trigger" : note;

        // Only an instance that will run states when it started. One that is
        // queued, skipped or failed never began.
        if (dispatch) {
            inst.started_at = std::chrono::system_clock::now();
        }

        service::report_instance_service inst_svc(tenant_ctx);
        inst_svc.save_instance(inst);

        const auto inst_id_str = boost::uuids::to_string(inst.id);
        BOOST_LOG_SEV(report_instance_trigger_handler_lg(), info)
            << "Created report instance " << inst_id_str << " for definition " << definition_id
            << (dispatch ? " and dispatched its workflow" : " without dispatching a workflow");

        if (dispatch) {
            dispatch_workflow(req, definition_id, inst_id_str, rg, msg);
        }

        response.result.code = dispatch ? "triggered" : "not_dispatched";
        response.result.message = std::format("Report instance {} created.", inst_id_str);
    }

    /**
     * @brief The id of an instance of this definition that has not finished.
     *
     * Read through the database rather than the generated repository, which
     * offers no filter by definition or by state.
     */
    std::optional<std::string> find_in_flight(const std::string& tenant,
                                              const std::string& definition_id) {
        const auto rows = ores::database::repository::execute_parameterized_string_query(
            ctx_,
            "SELECT coalesce(ores_reporting_in_flight_instance_fn($1::uuid, $2::uuid)::text, '')",
            {tenant, definition_id},
            report_instance_trigger_handler_lg(),
            "Reading the in-flight report instance");
        if (rows.empty() || rows.front().empty()) {
            return std::nullopt;
        }
        return rows.front();
    }

    void dispatch_workflow(const trigger_report_instance_request& req,
                           const std::string& definition_id,
                           const std::string& instance_id,
                           boost::uuids::random_generator& rg,
                           const ores::nats::message& msg) {
        report_execution_request exec_req{.report_instance_id = instance_id,
                                          .definition_id = definition_id,
                                          .tenant_id = boost::uuids::to_string(req.tenant_id),
                                          .correlation_id = instance_id};
        ores::workflow::messaging::start_workflow_message swm{
            .type = "report_execution_workflow",
            .tenant_id = boost::uuids::to_string(req.tenant_id),
            .request_json = rfl::json::write(exec_req),
            .correlation_id = instance_id,
            .instance_id = boost::uuids::to_string(rg())};
        nats_.js_publish(ores::workflow::messaging::start_workflow_message::nats_subject,
                         ores::nats::default_wire_codec().encode(swm),
                         ores::nats::service::forwarded_caller_headers(msg));
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    ores::workflow::service::fsm_state_map instance_states_;
};

} // namespace ores::reporting::messaging

#endif
