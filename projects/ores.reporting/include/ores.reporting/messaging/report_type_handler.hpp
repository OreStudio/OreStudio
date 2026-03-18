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
#ifndef ORES_REPORTING_MESSAGING_REPORT_TYPE_HANDLER_HPP
#define ORES_REPORTING_MESSAGING_REPORT_TYPE_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.reporting/messaging/report_type_protocol.hpp"
#include "ores.reporting/service/report_type_service.hpp"

namespace ores::reporting::messaging {

namespace {
inline auto& report_type_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.reporting.messaging.report_type_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using namespace ores::logging;

class report_type_handler {
public:
    report_type_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(report_type_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::report_type_service svc(ctx);
        get_report_types_response resp;
        try {
            resp.types = svc.list_types();
            resp.total_available_count =
                static_cast<int>(resp.types.size());
        } catch (...) {}
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(report_type_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(report_type_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (auto req = decode<save_report_type_request>(msg)) {
            service::report_type_service svc(ctx);
            try {
                stamp(req->type, ctx);
                svc.save_type(req->type);
                reply(nats_, msg, save_report_type_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_report_type_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_type_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(report_type_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void del(ores::nats::message msg) {
        BOOST_LOG_SEV(report_type_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (auto req = decode<delete_report_type_request>(msg)) {
            service::report_type_service svc(ctx);
            try {
                for (const auto& code : req->codes)
                    svc.remove_type(code);
                reply(nats_, msg,
                    delete_report_type_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, delete_report_type_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_type_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(report_type_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(report_type_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (auto req = decode<get_report_type_history_request>(msg)) {
            service::report_type_service svc(ctx);
            try {
                auto hist = svc.get_type_history(req->code);
                reply(nats_, msg, get_report_type_history_response{
                    .success = true,
                    .history = std::move(hist)});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_report_type_history_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(report_type_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(report_type_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::reporting::messaging

#endif
