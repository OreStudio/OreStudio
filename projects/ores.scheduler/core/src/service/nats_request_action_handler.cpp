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
#include "ores.scheduler.core/service/nats_request_action_handler.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.scheduler.core/service/action_payloads.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <string>

namespace ores::scheduler::service {

using namespace ores::logging;

namespace {

auto& nats_request_action_handler_lg() {
    static auto instance = make_logger("ores.scheduler.service.nats_request_action_handler");
    return instance;
}

}

nats_request_action_handler::nats_request_action_handler(
    ores::nats::service::nats_client& svc_nats)
    : svc_nats_(svc_nats) {}

boost::asio::awaitable<std::expected<void, std::string>>
nats_request_action_handler::execute(const action_context& ctx) {
    BOOST_LOG_SEV(nats_request_action_handler_lg(), debug)
        << "Executing NATS request action for job: " << ctx.job.job_name;
    try {
        nats_request_payload parsed;
        try {
            parsed =
                utility::codec::object::read_json<nats_request_payload>(ctx.job.action_payload);
        } catch (const std::exception& e) {
            const auto msg =
                "Failed to parse action_payload for job '" + ctx.job.job_name + "': " + e.what();
            BOOST_LOG_SEV(nats_request_action_handler_lg(), error) << msg;
            co_return std::unexpected(msg);
        }

        const auto& subject = parsed.subject;
        if (subject.empty()) {
            const auto msg =
                std::string("action_payload missing subject for job '") + ctx.job.job_name + "'";
            BOOST_LOG_SEV(nats_request_action_handler_lg(), error) << msg;
            co_return std::unexpected(msg);
        }

        // The answer carries the callee's own outcome, so its body is not read
        // here. An envelope refusal is the only failure this action can name,
        // and it must reach the job record rather than be discarded. The request
        // it sends has no members, so the callee's own no-member type reads it.
        const nats_request_body body;
        const auto reply =
            svc_nats_.authenticated_request(subject, ores::nats::default_wire_codec().encode(body));
        const auto err = reply.headers.find(std::string(ores::nats::headers::x_error));
        if (err != reply.headers.end()) {
            const auto msg = "Subject " + subject + " refused the request: " + err->second;
            BOOST_LOG_SEV(nats_request_action_handler_lg(), error)
                << msg << " (job: " << ctx.job.job_name << ")";
            co_return std::unexpected(msg);
        }

        BOOST_LOG_SEV(nats_request_action_handler_lg(), info)
            << "NATS request action succeeded for job: " << ctx.job.job_name
            << " (subject: " << subject << ")";
        co_return std::expected<void, std::string>{};
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(nats_request_action_handler_lg(), error)
            << "NATS request action failed for job: " << ctx.job.job_name << ": " << e.what();
        co_return std::unexpected(std::string(e.what()));
    }
}

}
