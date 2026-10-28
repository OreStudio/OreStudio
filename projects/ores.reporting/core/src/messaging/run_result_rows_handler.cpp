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
#include "ores.reporting.core/messaging/run_result_rows_handler.hpp"
#include "ores.reporting.core/repository/report_result_row_repository.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <rfl/json.hpp>

namespace ores::reporting::messaging {

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::reply;
using namespace ores::logging;

run_result_rows_handler::run_result_rows_handler(
    ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , verifier_(std::move(verifier)) {}

void run_result_rows_handler::list_report_result_rows(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

    auto req_ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
    if (!req_ctx) {
        error_reply(nats_, msg, req_ctx.error());
        return;
    }

    auto parsed = decode<list_report_result_rows_request>(msg);
    if (!parsed) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    const auto& request = *parsed;

    list_report_result_rows_response response;
    const auto run = request.filter.has_value() ? request.filter->report_instance_id :
                                                  std::optional<boost::uuids::uuid>();
    if (!run) {
        response.result.outcome = ores::utility::domain::outcome::ok;
        response.total = 0;
        reply(nats_, msg, response);
        return;
    }

    repository::report_result_row_repository repo;
    const auto rows = repo.read_by_run(*req_ctx, boost::uuids::to_string(*run));
    response.report_result_rows = rows;
    response.total = rows.size();
    response.result.outcome = ores::utility::domain::outcome::ok;
    reply(nats_, msg, response);
}

}
