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
#ifndef ORES_REPORTING_MESSAGING_REPORT_DEFINITION_HANDLER_HPP
#define ORES_REPORTING_MESSAGING_REPORT_DEFINITION_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.reporting.core/service/report_definition_service.hpp"
#include "ores.reporting.core/service/report_scheduling_service.hpp"

namespace ores::reporting::messaging {

namespace {
inline auto& report_definition_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.reporting.messaging.report_definition_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using namespace ores::logging;

class report_definition_handler {
public:
    report_definition_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(report_definition_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::report_definition_service svc(ctx);
        get_report_definitions_response resp;
        try {
            resp.definitions = svc.list_definitions();
            resp.total_available_count =
                static_cast<int>(resp.definitions.size());
        } catch (...) {}
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(report_definition_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(report_definition_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (auto req = decode<save_report_definition_request>(msg)) {
            service::report_definition_service svc(ctx);
            try {
                stamp(req->definition, ctx);
                svc.save_definition(req->definition);
                reply(nats_, msg,
                    save_report_definition_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_report_definition_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_definition_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(report_definition_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void del(ores::nats::message msg) {
        BOOST_LOG_SEV(report_definition_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (auto req = decode<delete_report_definition_request>(msg)) {
            service::report_definition_service svc(ctx);
            try {
                for (const auto& id : req->ids)
                    svc.remove_definition(id);
                reply(nats_, msg,
                    delete_report_definition_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, delete_report_definition_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_definition_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(report_definition_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(report_definition_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (auto req = decode<get_report_definition_history_request>(msg)) {
            service::report_definition_service svc(ctx);
            try {
                auto hist = svc.get_definition_history(req->id);
                reply(nats_, msg, get_report_definition_history_response{
                    .success = true,
                    .history = std::move(hist)});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_report_definition_history_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_definition_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(report_definition_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void schedule(ores::nats::message msg) {
        BOOST_LOG_SEV(report_definition_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (auto req = decode<schedule_report_definitions_request>(msg)) {
            service::report_definition_service svc(ctx);
            service::report_scheduling_service scheduler(ctx_, nats_);
            int scheduled_count = 0;
            try {
                for (const auto& id : req->ids) {
                    auto def = svc.find_definition(id);
                    if (!def) continue;
                    if (scheduler.schedule_one(*def, req->performed_by))
                        ++scheduled_count;
                }
                reply(nats_, msg, schedule_report_definitions_response{
                    .success = true,
                    .scheduled_count = scheduled_count});
            } catch (const std::exception& e) {
                reply(nats_, msg, schedule_report_definitions_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_definition_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(report_definition_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void unschedule(ores::nats::message msg) {
        BOOST_LOG_SEV(report_definition_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (auto req = decode<unschedule_report_definitions_request>(msg)) {
            service::report_definition_service svc(ctx);
            service::report_scheduling_service scheduler(ctx_, nats_);
            int unscheduled_count = 0;
            try {
                for (const auto& id : req->ids) {
                    auto def = svc.find_definition(id);
                    if (!def) continue;
                    if (scheduler.unschedule_one(*def, req->performed_by))
                        ++unscheduled_count;
                }
                reply(nats_, msg, unschedule_report_definitions_response{
                    .success = true,
                    .unscheduled_count = unscheduled_count});
            } catch (const std::exception& e) {
                reply(nats_, msg, unschedule_report_definitions_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_definition_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(report_definition_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::reporting::messaging

#endif
