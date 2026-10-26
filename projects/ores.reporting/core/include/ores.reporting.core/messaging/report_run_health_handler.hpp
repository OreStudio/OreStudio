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
#ifndef ORES_REPORTING_CORE_MESSAGING_REPORT_RUN_HEALTH_HANDLER_HPP
#define ORES_REPORTING_CORE_MESSAGING_REPORT_RUN_HEALTH_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::reporting::messaging {

namespace {
inline auto& report_run_health_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.reporting.messaging.report_run_health_handler");
    return instance;
}
} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief Projects the shared health read onto the shell's one-run answer.
 *
 * The read answers rows for every run it could see, and the operation names
 * one. A read that answers no row is a run that does not exist or is not the
 * session's to read, and the projection says so rather than answering an
 * empty word that a caller would read as a state.
 */
inline get_run_health_response project_run_health(const get_report_instance_health_response& read) {
    get_run_health_response response;
    response.result = read.result;
    if (response.result.outcome != ores::utility::domain::outcome::ok)
        return response;
    if (read.health.empty()) {
        response.result = ores::database::domain::refuse(
            ores::database::domain::outcome_code::not_found, {.entity = "report instance"});
        return response;
    }
    response.state_name = read.health.front().state_name;
    response.health = read.health.front().health;
    return response;
}

/**
 * @brief Hand-crafted NATS handler for a run's health, as the shell asks it.
 *
 * The operation names one run, while the read the web asks states the health of
 * many, so this handler answers the one by calling that same read with one id.
 * The word is the service's to derive; the handler projects the row and nothing
 * else, so the shell and the web cannot disagree about a run.
 */
class report_run_health_handler {
public:
    report_run_health_handler(ores::nats::service::client& nats,
                              ores::database::context ctx,
                              std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves reporting.v1.ops.get_run_health.
     *
     * A read of one run, so it is gated by the same permission the generated
     * run reads are gated by.
     */
    void get_run_health(ores::nats::message msg) {
        BOOST_LOG_SEV(report_run_health_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            report_run_health_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "reporting::report_instances:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_run_health_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(report_run_health_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        service::report_instance_service svc(req_ctx);
        try {
            get_report_instance_health_request read;
            read.instance_ids.push_back(req->report_instance_id);
            const auto response = project_run_health(svc.get_report_instance_health(read));
            BOOST_LOG_SEV(report_run_health_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(report_run_health_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_run_health_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::reporting::messaging

#endif
