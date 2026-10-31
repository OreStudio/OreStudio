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
#ifndef ORES_REPORTING_CORE_MESSAGING_REPORT_CUT_RESOLUTION_HANDLER_HPP
#define ORES_REPORTING_CORE_MESSAGING_REPORT_CUT_RESOLUTION_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/repository/cut_kind_repository.hpp"
#include "ores.reporting.core/service/cut_resolution_service.hpp"
#include "ores.reporting.core/service/cut_state.hpp"
#include "ores.reporting.core/service/report_definition_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string>

namespace ores::reporting::messaging {

namespace {
inline auto& report_cut_resolution_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.reporting.messaging.report_cut_resolution_handler");
    return instance;
}
} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief The parts of the body a run states when it asks for a body to be minted.
 *
 * Empty strings are parts the assembly could not fill, which is what refuses a
 * run that asked for a body its kind mints.
 */
inline service::cut_body body_of(const resolve_cut_request& request) {
    const auto id_of = [](const std::string& text) -> std::optional<boost::uuids::uuid> {
        if (text.empty())
            return std::nullopt;
        return boost::uuids::string_generator()(text);
    };
    const auto text_of = [](const std::string& value) -> std::optional<std::string> {
        if (value.empty())
            return std::nullopt;
        return value;
    };

    service::cut_body body;
    body.market_data_snapshot_id = id_of(request.market_data_snapshot_id);
    body.deal_population_id = id_of(request.deal_population_id);
    body.reference_data_version = text_of(request.reference_data_version);
    body.engine_version = text_of(request.engine_version);
    body.code_version = text_of(request.code_version);
    return body;
}

/**
 * @brief Answers the cut a report's selection resolves to, for a preview or a run.
 *
 * The selection is read from the definition and not from the caller, because a
 * caller that stated the selection could state one the report does not hold.
 * The covered date arrives from the run settings: the covered date says which
 * cut to look for, and the cut states its own instant.
 *
 * A selection that resolves to nothing is a successful call that refuses the
 * run, so the sentence is the answer and not an error.
 */
inline resolve_cut_response apply_cut_resolution(service::report_definition_service& definitions,
                                                 service::cut_resolution_service& cuts,
                                                 const resolve_cut_request& request,
                                                 const ores::database::context& ctx) {
    resolve_cut_response response;
    using ores::database::domain::outcome_code;
    using ores::database::domain::refuse;

    const auto definition = definitions.get_definition(request.report_definition_id);
    if (!definition) {
        response.result = refuse(outcome_code::not_found, {.entity = "report definition"});
        return response;
    }

    // A run writes the body it mints into a party's cuts, so it must act in one.
    // A preview writes nothing and so needs no party.
    if (!request.preview && !ctx.party_id()) {
        response.result = refuse(outcome_code::party_required);
        return response;
    }

    service::cut_selection selection;
    selection.kind_code = definition->cut_kind_code.value_or(std::string());
    if (definition->cut_id)
        selection.cut_id = definition->cut_id;

    if (selection.kind_code.empty()) {
        response.refusal =
            "The report selects no cut, so a run prices from no body. Select a kind of cut or a "
            "cut on the report's definition.";
        return response;
    }

    service::cut_resolution_context resolution;
    resolution.covered_date = ores::platform::time::datetime::from_iso8601_utc(
        request.covered_date + "T00:00:00Z");
    resolution.scope_code = request.scope_code;
    resolution.business_centre_code = request.business_centre_code;
    resolution.party_id = ctx.party_id().value_or(boost::uuids::uuid{});
    resolution.run_instant = std::chrono::system_clock::now();
    resolution.taken_by = ctx.service_account();
    resolution.body = body_of(request);
    resolution.preview = request.preview;

    const auto resolution_result = request.preview ? cuts.preview(selection, resolution)
                                                   : cuts.resolve(selection, resolution);

    if (!resolution_result.ok()) {
        response.refusal = resolution_result.refusal;
        return response;
    }

    if (resolution_result.cut) {
        const auto& cut = *resolution_result.cut;
        response.cut_id = boost::uuids::to_string(cut.id);
        response.cut_kind_code = cut.kind_code;
        response.cut_timepoint = ores::platform::time::datetime::to_iso8601_utc(cut.timepoint);
        response.cut_state = std::string(service::to_string(service::derive_state(cut, 0)));

        repository::cut_kind_repository kinds;
        const auto kinds_held = kinds.read_latest(ctx, cut.kind_code);
        if (!kinds_held.empty())
            response.cut_label = service::cut_label_of(cut, kinds_held.front());
        return response;
    }

    const auto& plan = *resolution_result.mint;
    response.cut_kind_code = plan.kind_code;
    response.cut_timepoint = ores::platform::time::datetime::to_iso8601_utc(plan.timepoint);
    return response;
}

/**
 * @brief Hand-crafted NATS handler for the cut a report's selection resolves to.
 *
 * The rules are a function and this serves them: the preview on the report's
 * page and the run that prices read one answer, because a preview computed by a
 * second code path is a preview of that path.
 */
class report_cut_resolution_handler {
public:
    report_cut_resolution_handler(ores::nats::service::client& nats,
                                  ores::database::context ctx,
                                  std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves reporting.v1.ops.resolve_cut.
     *
     * A read of the cuts and the definition, so it is gated by the permission
     * the cuts reads are gated by.
     */
    void resolve_cut(ores::nats::message msg) {
        BOOST_LOG_SEV(report_cut_resolution_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            report_cut_resolution_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "reporting::cuts:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<resolve_cut_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(report_cut_resolution_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        service::report_definition_service definitions(req_ctx);
        service::cut_resolution_service cuts(req_ctx);
        try {
            auto response = apply_cut_resolution(definitions, cuts, *req, req_ctx);
            BOOST_LOG_SEV(report_cut_resolution_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(report_cut_resolution_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resolve_cut_response failure;
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
