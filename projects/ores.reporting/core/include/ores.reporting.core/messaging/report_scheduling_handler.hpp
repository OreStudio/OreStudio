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
#ifndef ORES_REPORTING_CORE_MESSAGING_REPORT_SCHEDULING_HANDLER_HPP
#define ORES_REPORTING_CORE_MESSAGING_REPORT_SCHEDULING_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.reporting.api/messaging/report_scheduling_protocol.hpp"
#include "ores.reporting.core/service/report_definition_service.hpp"
#include "ores.reporting.core/service/report_scheduling_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

namespace {
inline auto& report_scheduling_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.reporting.messaging.report_scheduling_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::delegated_actor;
using namespace ores::logging;

/**
 * @brief Hand-crafted NATS handler for report definition scheduling operations.
 *
 * Lives outside the codegen-generated handler so that the generated
 * report_definition_handler remains a pure CRUD shell (zero-diff
 * invariant). schedule and unschedule are not standard CRUD — they
 * orchestrate across the scheduler service — and their presence in the
 * generated handler caused codegen drift.
 */
class report_scheduling_handler {
public:
    report_scheduling_handler(ores::nats::service::client& nats,
                              ores::database::context ctx,
                              std::optional<ores::security::jwt::jwt_authenticator> verifier,
                              ores::nats::service::nats_client& svc_nats)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , svc_nats_(svc_nats) {}

    void schedule(ores::nats::message msg) {
        BOOST_LOG_SEV(report_scheduling_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "reporting::report_definitions:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        if (auto req = decode<schedule_report_definitions_request>(msg)) {
            service::report_definition_service svc(req_ctx);
            auto delegated = svc_nats_.with_delegation(ores::nats::service::extract_bearer(msg));
            service::report_scheduling_service scheduler(ctx_, delegated);
            const auto& actor = delegated_actor(req_ctx);
            int scheduled_count = 0;
            std::vector<std::string> failed_ids;
            std::string first_error;
            for (const auto& id : req->ids) {
                try {
                    auto def = svc.get_definition(id);
                    if (!def)
                        continue;
                    auto result = scheduler.schedule_one(*def, actor);
                    if (!result) {
                        failed_ids.push_back(id);
                        if (first_error.empty())
                            first_error = result.error();
                    } else if (*result)
                        ++scheduled_count;
                } catch (const std::exception& e) {
                    failed_ids.push_back(id);
                    if (first_error.empty())
                        first_error = e.what();
                }
            }
            reply(nats_,
                  msg,
                  schedule_report_definitions_response{.success = failed_ids.empty(),
                                                       .message = first_error,
                                                       .scheduled_count = scheduled_count,
                                                       .failed_ids = std::move(failed_ids)});
        } else {
            BOOST_LOG_SEV(report_scheduling_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(report_scheduling_handler_lg(), debug) << "Completed " << msg.subject;
    }

    void unschedule(ores::nats::message msg) {
        BOOST_LOG_SEV(report_scheduling_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "reporting::report_definitions:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        if (auto req = decode<unschedule_report_definitions_request>(msg)) {
            service::report_definition_service svc(req_ctx);
            auto delegated = svc_nats_.with_delegation(ores::nats::service::extract_bearer(msg));
            service::report_scheduling_service scheduler(ctx_, delegated);
            const auto& actor = delegated_actor(req_ctx);
            int unscheduled_count = 0;
            std::vector<std::string> failed_ids;
            std::string first_error;
            for (const auto& id : req->ids) {
                try {
                    auto def = svc.get_definition(id);
                    if (!def)
                        continue;
                    auto result = scheduler.unschedule_one(*def, actor);
                    if (!result) {
                        failed_ids.push_back(id);
                        if (first_error.empty())
                            first_error = result.error();
                    } else if (*result)
                        ++unscheduled_count;
                } catch (const std::exception& e) {
                    failed_ids.push_back(id);
                    if (first_error.empty())
                        first_error = e.what();
                }
            }
            reply(nats_,
                  msg,
                  unschedule_report_definitions_response{.success = failed_ids.empty(),
                                                         .message = first_error,
                                                         .unscheduled_count = unscheduled_count,
                                                         .failed_ids = std::move(failed_ids)});
        } else {
            BOOST_LOG_SEV(report_scheduling_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(report_scheduling_handler_lg(), debug) << "Completed " << msg.subject;
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    ores::nats::service::nats_client& svc_nats_;
};

} // namespace ores::reporting::messaging

#endif
