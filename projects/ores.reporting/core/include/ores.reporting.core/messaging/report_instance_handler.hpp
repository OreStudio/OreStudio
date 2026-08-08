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
#ifndef ORES_REPORTING_CORE_MESSAGING_REPORT_INSTANCE_HANDLER_HPP
#define ORES_REPORTING_CORE_MESSAGING_REPORT_INSTANCE_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.reporting.api/messaging/report_execution_protocol.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.api/messaging/report_scheduling_protocol.hpp"
#include "ores.reporting.core/service/report_definition_service.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.workflow.api/messaging/workflow_events.hpp"
#include "ores.workflow.core/service/fsm_state_map.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <rfl/json.hpp>

namespace ores::reporting::messaging {

namespace {
inline auto& report_instance_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.reporting.messaging.report_instance_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for report instance operations.
 */
class report_instance_handler {
public:
    report_instance_handler(ores::nats::service::client& nats,
                            ores::database::context ctx,
                            std::optional<ores::security::jwt::jwt_authenticator> verifier,
                            ores::workflow::service::fsm_state_map instance_states)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , instance_states_(std::move(instance_states)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(report_instance_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::report_instance_service svc(req_ctx);
        get_report_instances_response resp;
        if (auto req = decode<get_report_instances_request>(msg)) {
            try {
                resp.instances = svc.list_instances(req->offset, req->limit);
                resp.total_available_count = static_cast<int>(svc.count_instances());
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(report_instance_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
        } else {
            BOOST_LOG_SEV(report_instance_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        BOOST_LOG_SEV(report_instance_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(report_instance_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "reporting::report_instances:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::report_instance_service svc(req_ctx);
        if (auto req = decode<save_report_instance_request>(msg)) {
            try {
                svc.save_instance(req->data);
                BOOST_LOG_SEV(report_instance_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_, msg, save_report_instance_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(report_instance_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      save_report_instance_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_instance_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(report_instance_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::report_instance_service svc(req_ctx);
        if (auto req = decode<get_report_instance_history_request>(msg)) {
            try {
                auto hist = svc.get_instance_history(req->id);
                BOOST_LOG_SEV(report_instance_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_,
                      msg,
                      get_report_instance_history_response{.history = std::move(hist),
                                                           .success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(report_instance_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      get_report_instance_history_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_instance_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(report_instance_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "reporting::report_instances:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::report_instance_service svc(req_ctx);
        if (auto req = decode<delete_report_instance_request>(msg)) {
            try {
                svc.delete_instances(req->ids);
                BOOST_LOG_SEV(report_instance_handler_lg(), debug) << "Completed " << msg.subject;
                reply(nats_, msg, delete_report_instance_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(report_instance_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_,
                      msg,
                      delete_report_instance_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_instance_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void trigger(ores::nats::message msg) {
        BOOST_LOG_SEV(report_instance_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (auto trigger_msg = decode<trigger_report_instance_message>(msg)) {
            try {
                service::report_definition_service def_svc(req_ctx);
                const auto def = def_svc.get_definition(trigger_msg->report_definition_id);
                if (!def) {
                    BOOST_LOG_SEV(report_instance_handler_lg(), warn)
                        << "Definition not found: " << trigger_msg->report_definition_id;
                    return;
                }

                service::report_instance_service inst_svc(req_ctx);
                const auto active = inst_svc.list_instances(0, 1);
                const bool has_active = !active.empty();

                boost::uuids::string_generator uuid_gen;
                const auto pending_id = instance_states_.require("pending");
                boost::uuids::uuid initial_state = pending_id;
                bool should_dispatch = true;

                if (!has_active) {
                    initial_state = pending_id;
                    should_dispatch = true;
                } else if (def->concurrency_policy == "queue") {
                    initial_state = instance_states_.require("queued");
                    should_dispatch = false;
                } else if (def->concurrency_policy == "skip") {
                    initial_state = instance_states_.require("skipped");
                    should_dispatch = false;
                } else {
                    initial_state = instance_states_.require("failed");
                    should_dispatch = false;
                }

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
                    << "Created report instance " << inst_id_str << " for definition " << def->id
                    << " state="
                    << (should_dispatch ? "pending"
                                        : (def->concurrency_policy == "queue"   ? "queued"
                                           : def->concurrency_policy == "skip" ? "skipped"
                                                                               : def->concurrency_policy))
                    << " (job_instance_id=" << trigger_msg->job_instance_id << ")";

                if (should_dispatch) {
                    const auto wf_instance_id = boost::uuids::to_string(rg());

                    report_execution_request exec_req{.report_instance_id = inst_id_str,
                                                      .definition_id =
                                                          trigger_msg->report_definition_id,
                                                      .tenant_id = trigger_msg->tenant_id,
                                                      .correlation_id = inst_id_str};

                    ores::workflow::messaging::start_workflow_message swm{
                        .type = "report_execution_workflow",
                        .tenant_id = trigger_msg->tenant_id,
                        .request_json = rfl::json::write(exec_req),
                        .correlation_id = inst_id_str,
                        .instance_id = wf_instance_id};

                    nats_.js_publish(ores::workflow::messaging::start_workflow_message::nats_subject,
                                     ores::nats::default_wire_codec().encode(swm));

                    BOOST_LOG_SEV(report_instance_handler_lg(), info)
                        << "Dispatched report_execution_workflow for instance " << inst_id_str
                        << " wf_instance=" << wf_instance_id;
                }
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(report_instance_handler_lg(), error)
                    << "Failed to create report instance for trigger: " << e.what();
            }
        } else {
            BOOST_LOG_SEV(report_instance_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
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
