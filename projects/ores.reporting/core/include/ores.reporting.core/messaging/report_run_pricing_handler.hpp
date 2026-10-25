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
#ifndef ORES_REPORTING_CORE_MESSAGING_REPORT_RUN_PRICING_HANDLER_HPP
#define ORES_REPORTING_CORE_MESSAGING_REPORT_RUN_PRICING_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <chrono>
#include <optional>
#include <string>

namespace ores::reporting::messaging {

namespace {
inline auto& report_run_pricing_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.reporting.messaging.report_run_pricing_handler");
    return instance;
}
} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief Records the date a run priced at, and why it moved, on the run.
 *
 * The date arrives as the day the packaging step used, and it is stored on the
 * run as an instant at midnight UTC, because the engine prices a day and the
 * time of day the step ran is not part of the answer. A run the session cannot
 * read answers the same missing result as a run that does not exist: the
 * row-level policy on the table decides which of the two it is, and this
 * function never distinguishes them.
 */
inline record_run_pricing_response apply_run_pricing(
    service::report_instance_service& instances, const record_run_pricing_request& request) {
    record_run_pricing_response response;
    using ores::database::domain::outcome_code;
    using ores::database::domain::refuse;

    const auto run = instances.get_instance(request.report_instance_id);
    if (!run) {
        response.result = refuse(outcome_code::not_found, {.entity = "report instance"});
        return response;
    }

    std::chrono::system_clock::time_point priced_at;
    try {
        priced_at = ores::platform::time::datetime::from_utc_format(request.priced_at, "%Y-%m-%d");
    } catch (const std::exception&) {
        response.result = refuse(outcome_code::as_of_invalid, {.value = request.priced_at});
        return response;
    }

    auto updated = *run;
    updated.priced_at = priced_at;
    updated.price_note = request.price_note;
    updated.change_reason_code = std::string(ores::service::messaging::change_reasons::update);
    updated.change_commentary = "Recorded the date the run priced at";
    instances.save_instance(updated);
    return response;
}

/**
 * @brief Hand-crafted NATS handler for the date a run priced at.
 *
 * The packaging step is the one place that knows both the date the run asked
 * for and the date the gathered data carries, so it states the date it used
 * on the run. The operation is a write of the run, so it is gated by the same
 * permission the generated run writes are gated by.
 */
class report_run_pricing_handler {
public:
    report_run_pricing_handler(ores::nats::service::client& nats,
                               ores::database::context ctx,
                               std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves reporting.v1.ops.record_run_pricing.
     *
     * A write of one run, so it is gated by the same permission the generated
     * run writes are gated by.
     */
    void record_run_pricing(ores::nats::message msg) {
        BOOST_LOG_SEV(report_run_pricing_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            report_run_pricing_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "reporting::report_instances:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<record_run_pricing_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(report_run_pricing_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        service::report_instance_service svc(req_ctx);
        try {
            auto response = apply_run_pricing(svc, *req);
            BOOST_LOG_SEV(report_run_pricing_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(report_run_pricing_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            record_run_pricing_response failure;
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
