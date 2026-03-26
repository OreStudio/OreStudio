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

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.reporting.core/service/report_definition_service.hpp"

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
using namespace ores::logging;

class report_instance_handler {
public:
    report_instance_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

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

    void del(ores::nats::message msg) {
        BOOST_LOG_SEV(report_instance_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
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
     * when a report job fires. Looks up the report definition and creates a
     * new report_instance. No reply is sent (publish, not request/reply).
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
            // Create a tenant-specific context from the tenant_id in the message.
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

            // Look up the report definition.
            service::report_definition_service def_svc(tenant_ctx);
            const auto def = def_svc.find_definition(
                trigger_msg->report_definition_id);
            if (!def) {
                BOOST_LOG_SEV(report_instance_handler_lg(), error)
                    << "Report definition not found: "
                    << trigger_msg->report_definition_id;
                return;
            }

            // Create and save the report instance.
            static boost::uuids::random_generator rg;
            domain::report_instance inst;
            inst.id = rg();
            inst.tenant_id = def->tenant_id;
            inst.party_id = def->party_id;
            inst.definition_id = def->id;
            inst.name = def->name;
            inst.description = def->description;
            inst.trigger_run_id = trigger_msg->job_instance_id;
            inst.modified_by = ctx_.service_account();
            inst.performed_by = ctx_.service_account();
            inst.change_reason_code = "system.scheduler_trigger";
            inst.change_commentary = "Created by scheduler trigger";

            service::report_instance_service inst_svc(tenant_ctx);
            inst_svc.save_instance(inst);

            BOOST_LOG_SEV(report_instance_handler_lg(), info)
                << "Created report instance " << inst.id
                << " for definition " << def->id
                << " (job_instance_id=" << trigger_msg->job_instance_id << ")";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(report_instance_handler_lg(), error)
                << "Failed to create report instance for trigger: " << e.what();
        }
        BOOST_LOG_SEV(report_instance_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::reporting::messaging

#endif
