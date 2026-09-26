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
#include "ores.scheduler.core/service/nats_publish_action_handler.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cstdint>
#include <optional>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <string>

namespace ores::scheduler::service {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.scheduler.service.nats_publish_action_handler");
    return instance;
}

// Fields read from job_definition.action_payload.
struct nats_publish_payload {
    std::string subject;
    std::optional<std::string> report_definition_id;
    std::optional<std::string> tenant_id;
};

// The body published to the subject on each firing.
//
// Its field order and types are the reporting operation's
// trigger_report_instance_request: the definition, the tenant, then the run.
// A report trigger and its receiver are two ends of one contract, and the
// codec is positional, so the order is the contract.
struct nats_trigger_body {
    std::string report_definition_id;
    std::string tenant_id;
    std::int64_t job_instance_id = 0;
};

} // anonymous namespace

nats_publish_action_handler::nats_publish_action_handler(
    ores::nats::service::client& nats,
    ores::nats::service::nats_client& svc_nats)
    : nats_(nats)
    , svc_nats_(svc_nats) {}

boost::asio::awaitable<std::expected<void, std::string>>
nats_publish_action_handler::execute(const action_context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Executing NATS publish action for job: " << ctx.job.job_name;
    try {
        auto parsed = rfl::json::read<nats_publish_payload>(ctx.job.action_payload);
        if (!parsed) {
            const auto msg = "Failed to parse action_payload for job '" + ctx.job.job_name +
                             "': " + parsed.error().what();
            BOOST_LOG_SEV(lg(), error) << msg;
            co_return std::unexpected(msg);
        }

        const auto& subject = parsed->subject;
        if (subject.empty()) {
            const auto msg =
                std::string("action_payload missing subject for job '") + ctx.job.job_name + "'";
            BOOST_LOG_SEV(lg(), error) << msg;
            co_return std::unexpected(msg);
        }

        const nats_trigger_body body{.report_definition_id =
                                         parsed->report_definition_id.value_or(""),
                                     .tenant_id = parsed->tenant_id.value_or(""),
                                     .job_instance_id = ctx.inst_id};

        // A job that names no report definition is a notification nothing
        // answers, so it stays fire-and-forget.
        if (!parsed->report_definition_id) {
            nats_.publish(subject, ores::nats::default_wire_codec().encode(body));
            BOOST_LOG_SEV(lg(), info)
                << "NATS publish action succeeded for job: " << ctx.job.job_name
                << " (subject: " << subject << ")";
            co_return std::expected<void, std::string>{};
        }

        // A report trigger is a request: the reporting service answers, and a
        // refusal has to reach the job rather than being discarded. The reply
        // carries no subject of its own, so a rejection arrives as an X-Error
        // header.
        const auto reply = svc_nats_.authenticated_request(
            subject, ores::nats::default_wire_codec().encode(body));
        const auto err = reply.headers.find(std::string(ores::nats::headers::x_error));
        if (err != reply.headers.end()) {
            const auto msg = "Subject " + subject + " refused the trigger: " + err->second;
            BOOST_LOG_SEV(lg(), error) << msg << " (job: " << ctx.job.job_name << ")";
            co_return std::unexpected(msg);
        }

        BOOST_LOG_SEV(lg(), info) << "NATS publish action succeeded for job: " << ctx.job.job_name
                                  << " (subject: " << subject << ")";
        co_return std::expected<void, std::string>{};
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "NATS publish action failed for job: " << ctx.job.job_name << ": " << e.what();
        co_return std::unexpected(std::string(e.what()));
    }
}

} // namespace ores::scheduler::service
