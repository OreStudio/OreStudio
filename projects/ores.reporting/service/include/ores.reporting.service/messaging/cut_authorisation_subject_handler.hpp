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
#ifndef ORES_REPORTING_SERVICE_MESSAGING_CUT_AUTHORISATION_SUBJECT_HANDLER_HPP
#define ORES_REPORTING_SERVICE_MESSAGING_CUT_AUTHORISATION_SUBJECT_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/service/cut_read_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <exception>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace ores::reporting::messaging {

/**
 * @brief Serves reporting.v1.ops.list_cut_authorisation_subjects: what each cut
 * authorisation is about.
 *
 * A cut's authorisation is decided on the request page, and the request is the
 * inbox's record: it names neither the body nor its occasion. The subject is this
 * component's detail row, keyed by the request, and the cut the row names holds
 * the label, the kind, the instant, the scope and the counts. The page reads them
 * here rather than copying them into the generic record, because two copies of
 * one act drift.
 *
 * The read needs =reporting::cuts:read=, which is the permission a person reads a
 * cut with: a decider who may not see the body is told what the request is and
 * nothing about the body it hides.
 */
class cut_authorisation_subject_handler {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance = ores::logging::make_logger(
            "ores.reporting.messaging.cut_authorisation_subject_handler");
        return instance;
    }

public:
    cut_authorisation_subject_handler(ores::nats::service::client& nats,
                                      ores::database::context ctx,
                                      std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves reporting.v1.ops.list_cut_authorisation_subjects.
     */
    void list_subjects(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        BOOST_LOG_SEV(lg(), ores::logging::debug) << "Handling " << msg.subject;

        auto req_ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx) {
            ores::service::messaging::error_reply(nats_, msg, req_ctx.error());
            return;
        }
        auto req = ores::service::messaging::decode<list_cut_authorisation_subjects_request>(msg);
        if (!req) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        if (!ores::service::messaging::has_permission(*req_ctx, "reporting::cuts:read")) {
            ores::service::messaging::error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        try {
            // The caller states request identifiers, so a malformed one is the
            // caller's mistake and not a row the store refuses.
            std::vector<boost::uuids::uuid> request_ids;
            request_ids.reserve(req->request_ids.size());
            boost::uuids::string_generator parse;
            for (const auto& text : req->request_ids) {
                if (text.empty())
                    continue;
                try {
                    request_ids.push_back(parse(text));
                } catch (const std::exception&) {
                    BOOST_LOG_SEV(lg(), ores::logging::warn)
                        << msg.subject << " was asked for a malformed identifier: " << text;
                    ores::service::messaging::error_reply(
                        nats_, msg, ores::service::error_code::bad_request);
                    return;
                }
            }

            auto view = ores::reporting::service::cut_read_service(*req_ctx);
            list_cut_authorisation_subjects_response answer;
            for (const auto& subject : view.authorisation_subjects(request_ids)) {
                cut_authorisation_subject row;
                row.request_id = boost::uuids::to_string(subject.request_id);
                row.cut_id = boost::uuids::to_string(subject.cut_id);
                row.label = subject.label;
                row.kind_code = subject.kind_code;
                row.kind_name = subject.kind_name;
                row.timepoint = ores::platform::time::datetime::to_iso8601_utc(subject.timepoint);
                row.timepoint_timezone = subject.timepoint_timezone;
                row.scope_code = subject.scope_code;
                row.business_centre_code = subject.business_centre_code;
                row.state = std::string(ores::reporting::service::to_string(subject.state));
                row.deal_count = subject.deal_count;
                row.market_data_set_count = subject.market_data_set_count;
                row.gap_count = subject.gap_count;
                row.applied_by = subject.applied_by;
                if (subject.applied_at)
                    row.applied_at =
                        ores::platform::time::datetime::to_iso8601_utc(*subject.applied_at);
                answer.subjects.push_back(std::move(row));
            }
            answer.result = {.outcome = outcome::ok, .code = "", .message = "", .fields = {}};
            BOOST_LOG_SEV(lg(), ores::logging::debug)
                << "Completed " << msg.subject << " with " << answer.subjects.size() << " subject(s)";
            ores::service::messaging::reply(nats_, msg, answer);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), ores::logging::error) << msg.subject << " failed: " << e.what();
            ores::service::messaging::reply(
                nats_,
                msg,
                list_cut_authorisation_subjects_response{
                    .result = {.outcome = outcome::failed,
                               .code = "subjects_failed",
                               .message = "The cut authorisations could not be read.",
                               .fields = {}},
                    .subjects = {}});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
