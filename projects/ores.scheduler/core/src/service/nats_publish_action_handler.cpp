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
#include "ores.scheduler.core/service/action_payloads.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::scheduler::service {

using namespace ores::logging;

namespace {

auto& nats_publish_action_handler_lg() {
    static auto instance = make_logger("ores.scheduler.service.nats_publish_action_handler");
    return instance;
}

boost::uuids::uuid parse_uuid(const std::string& value, const std::string& what) {
    try {
        return boost::uuids::string_generator()(value);
    } catch (const std::exception&) {
        throw std::invalid_argument("action_payload names '" + value + "' as " + what +
                                    ", which is not an identifier");
    }
}


// Why the answering operation refused the request, or empty when it did not.
//
// Every operation answers with a result naming an outcome, and a refusal of the
// trigger itself is a failed outcome in the body rather than an envelope error.
// The subject and the response type are the action_payload's business, so this
// reads the common result and not the response. An answer it cannot read counts
// as a refusal: a firing nobody can account for is what the job record is for.
std::string refusal_in_reply(const std::vector<std::byte>& body) {
    if (body.empty())
        return "it answered with no body";

    const auto decoded = ores::nats::default_wire_codec().decode<reply_body>(body);
    if (!decoded)
        return "its answer could not be read: " + decoded.error().what();

    if (decoded->result.outcome == ores::utility::domain::outcome::ok)
        return {};

    return decoded->result.code + ": " + decoded->result.message;
}

} // anonymous namespace

nats_publish_action_handler::nats_publish_action_handler(ores::nats::service::client& nats,
                                                         ores::nats::service::nats_client& svc_nats)
    : nats_(nats)
    , svc_nats_(svc_nats) {}

boost::asio::awaitable<std::expected<void, std::string>>
nats_publish_action_handler::execute(const action_context& ctx) {
    BOOST_LOG_SEV(nats_publish_action_handler_lg(), debug)
        << "Executing NATS publish action for job: " << ctx.job.job_name;
    try {
        std::optional<nats_publish_payload> parsed;
        try {
            parsed =
                utility::codec::object::read_json<nats_publish_payload>(ctx.job.action_payload);
        } catch (const std::exception& e) {
            const auto msg =
                "Failed to parse action_payload for job '" + ctx.job.job_name + "': " + e.what();
            BOOST_LOG_SEV(nats_publish_action_handler_lg(), error) << msg;
            co_return std::unexpected(msg);
        }

        const auto& subject = parsed->subject;
        if (subject.empty()) {
            const auto msg =
                std::string("action_payload missing subject for job '") + ctx.job.job_name + "'";
            BOOST_LOG_SEV(nats_publish_action_handler_lg(), error) << msg;
            co_return std::unexpected(msg);
        }

        // A job that names no report definition is a notification nothing
        // answers: it states a subject and nothing this handler can place in a
        // report request, so it stays fire-and-forget and reads no identifiers.
        if (!parsed->report_definition_id) {
            nats_.publish(subject, {});
            BOOST_LOG_SEV(nats_publish_action_handler_lg(), info)
                << "NATS publish action succeeded for job: " << ctx.job.job_name
                << " (subject: " << subject << ")";
            co_return std::expected<void, std::string>{};
        }

        const nats_trigger_body body{
            .report_definition_id =
                parse_uuid(*parsed->report_definition_id, "report_definition_id"),
            .tenant_id = parse_uuid(parsed->tenant_id.value_or(""), "tenant_id"),
            .job_instance_id = ctx.inst_id,
            .kind = "production",
            .covered_date = "",
            .scope_code = "",
            .business_centre_code = ""};

        // A report trigger is a request: the reporting service answers, and a
        // refusal has to reach the job rather than being discarded. The reply
        // carries no subject of its own, so a rejection the reporting service
        // makes before reading the body arrives as an X-Error header.
        const auto reply =
            svc_nats_.authenticated_request(subject, ores::nats::default_wire_codec().encode(body));
        const auto err = reply.headers.find(std::string(ores::nats::headers::x_error));
        if (err != reply.headers.end()) {
            const auto msg = "Subject " + subject + " refused the trigger: " + err->second;
            BOOST_LOG_SEV(nats_publish_action_handler_lg(), error)
                << msg << " (job: " << ctx.job.job_name << ")";
            co_return std::unexpected(msg);
        }

        const auto refused = refusal_in_reply(reply.data);
        if (!refused.empty()) {
            const auto msg = "Subject " + subject + " refused the trigger: " + refused;
            BOOST_LOG_SEV(nats_publish_action_handler_lg(), error)
                << msg << " (job: " << ctx.job.job_name << ")";
            co_return std::unexpected(msg);
        }

        BOOST_LOG_SEV(nats_publish_action_handler_lg(), info)
            << "NATS publish action succeeded for job: " << ctx.job.job_name
            << " (subject: " << subject << ")";
        co_return std::expected<void, std::string>{};
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(nats_publish_action_handler_lg(), error)
            << "NATS publish action failed for job: " << ctx.job.job_name << ": " << e.what();
        co_return std::unexpected(std::string(e.what()));
    }
}

} // namespace ores::scheduler::service
