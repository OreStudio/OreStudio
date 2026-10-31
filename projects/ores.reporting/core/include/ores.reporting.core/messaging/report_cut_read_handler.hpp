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
#ifndef ORES_REPORTING_CORE_MESSAGING_REPORT_CUT_READ_HANDLER_HPP
#define ORES_REPORTING_CORE_MESSAGING_REPORT_CUT_READ_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/service/cut_read_service.hpp"
#include "ores.reporting.core/service/cut_state.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>

namespace ores::reporting::messaging {

namespace {
inline auto& report_cut_read_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.reporting.messaging.report_cut_read_handler");
    return instance;
}
} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;
using namespace ores::logging;

/** @brief An instant as the wire states it, or empty where the cut holds none. */
inline std::string instant_of(
    const std::optional<std::chrono::system_clock::time_point>& at) {
    if (!at)
        return {};
    return ores::platform::time::datetime::to_iso8601_utc(*at);
}

/** @brief One row of the list, as the wire states it. */
inline cut_row row_of(const service::cut_row_view& view) {
    cut_row row;
    row.cut_id = boost::uuids::to_string(view.cut.id);
    row.label = view.label;
    row.state = std::string(service::to_string(view.state));
    row.kind_code = view.cut.kind_code;
    row.kind_name = view.kind ? view.kind->name : std::string();
    row.timepoint = ores::platform::time::datetime::to_iso8601_utc(view.cut.timepoint);
    row.timepoint_timezone = view.cut.timepoint_timezone;
    row.scope_code = view.cut.scope_code;
    row.business_centre_code = view.cut.business_centre_code;
    row.taken_by = view.cut.taken_by;
    row.taken_at = instant_of(view.cut.taken_at);
    row.approved_by = view.cut.approved_by;
    row.approved_at = instant_of(view.cut.approved_at);
    row.refused_at = instant_of(view.cut.refused_at);
    row.requires_authorisation = view.cut.requires_authorisation;
    row.output_is_indicative = view.cut.output_is_indicative;
    row.take_ordinal = view.cut.take_ordinal;
    row.run_count = view.run_count;
    if (view.superseded_by)
        row.superseded_by_id = boost::uuids::to_string(*view.superseded_by);
    return row;
}

/**
 * @brief Hand-crafted NATS handler for the two reads the cut screens draw.
 *
 * The state and the label are functions of a cut's own facts, and the rules
 * live in the reporting service. The handler therefore projects what the read
 * derived and derives nothing, so the list, the record and the resolution
 * cannot disagree about a body.
 */
class report_cut_read_handler {
public:
    report_cut_read_handler(ores::nats::service::client& nats,
                            ores::database::context ctx,
                            std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves reporting.v1.ops.list_cut_rows.
     *
     * A read of the cuts a person sees, so it is gated by the permission the
     * cut's own reads are gated by. The party is the session's: a caller that
     * stated a party could read another party's bodies.
     */
    void list_cut_rows(ores::nats::message msg) {
        BOOST_LOG_SEV(report_cut_read_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            report_cut_read_handler_lg(), msg, req_ctx.tenant_id().to_string());
        auto req = decode<list_cut_rows_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(report_cut_read_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        if (!has_permission(req_ctx, "reporting::cuts:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        service::cut_read_service cuts(req_ctx);
        list_cut_rows_response response;
        try {
            const auto page = cuts.list(service::cut_list_query{.kind_code = req->kind_code,
                                                                .month = req->month,
                                                                .search = req->search,
                                                                .state = req->state});
            if (!page) {
                // A month a caller stated wrongly is the caller's fault, so the
                // read answers it as a refusal with its own sentence and the
                // BFF turns it into the bad request it is.
                BOOST_LOG_SEV(report_cut_read_handler_lg(), warn)
                    << msg.subject << ": " << page.error();
                response.result.outcome = ores::utility::domain::outcome::invalid;
                response.result.code = std::string(
                    ores::database::domain::to_string(
                        ores::database::domain::outcome_code::filter_range_invalid));
                response.result.message = page.error();
                reply(nats_, msg, response);
                return;
            }
            response.total = page->total;
            response.rows.reserve(page->rows.size());
            for (const auto& view : page->rows)
                response.rows.push_back(row_of(view));
            BOOST_LOG_SEV(report_cut_read_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(report_cut_read_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_cut_rows_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves reporting.v1.ops.get_cut_record.
     *
     * The same permission and the same derivation as the list, so a record says
     * what the row said.
     */
    void get_cut_record(ores::nats::message msg) {
        BOOST_LOG_SEV(report_cut_read_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            report_cut_read_handler_lg(), msg, req_ctx.tenant_id().to_string());
        auto req = decode<get_cut_record_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(report_cut_read_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        if (!has_permission(req_ctx, "reporting::cuts:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        service::cut_read_service cuts(req_ctx);
        get_cut_record_response response;
        try {
            const auto record = cuts.record(req->cut_id);
            if (!record) {
                response.result = ores::database::domain::refuse(
                    ores::database::domain::outcome_code::not_found, {.entity = "cut"});
                reply(nats_, msg, response);
                return;
            }
            response.cut = record->cut;
            if (record->kind)
                response.kind = *record->kind;
            response.label = record->label;
            response.state = std::string(service::to_string(record->state));
            if (record->superseded_by)
                response.superseded_by_id = boost::uuids::to_string(*record->superseded_by);
            response.gaps = record->gaps;
            response.run_count = record->run_count;
            BOOST_LOG_SEV(report_cut_read_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(report_cut_read_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_cut_record_response failure;
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

}

#endif
