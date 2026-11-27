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
#include "ores.marketdata.core/messaging/requirement_coverage_handler.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/messaging/party_required.hpp"
#include "ores.marketdata.core/repository/requirement_coverage_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <cstddef>
#include <exception>
#include <string>

namespace ores::marketdata::messaging {

namespace {
inline auto& requirement_coverage_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.requirement_coverage_handler");
    return instance;
}
}

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

void requirement_coverage_handler::get_requirement_coverage(ores::nats::message msg) {
    [[maybe_unused]] const auto correlation_id =
        log_handler_entry(requirement_coverage_handler_lg(), msg);
    auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
    if (!ctx_expected) {
        error_reply(nats_, msg, ctx_expected.error());
        return;
    }
    const auto& ctx = *ctx_expected;
    get_requirement_coverage_response resp;
    try {
        auto req = decode<get_requirement_coverage_request>(msg);
        if (!req) {
            resp.message = "Failed to decode request.";
            reply(nats_, msg, resp);
            return;
        }
        if (!names_a_party(ctx)) {
            resp.message = party_required_message();
            reply(nats_, msg, resp);
            return;
        }
        repository::requirement_coverage_repository coverage_repo;
        std::size_t gaps = 0;
        for (const auto& one : coverage_repo.read(ctx, req->requirements, req->as_of)) {
            if (!one.answered())
                ++gaps;
            resp.answers.push_back(requirement_answer{.oresmd_uri = one.oresmd_uri,
                                                      .series_id = one.series_id,
                                                      .gap_cause = one.gap_cause});
        }
        resp.success = true;
        resp.message = "Answered " + std::to_string(resp.answers.size() - gaps) + " of " +
                       std::to_string(resp.answers.size()) + " address(es).";
        ORES_LOG_SEV(requirement_coverage_handler_lg(), info)
            << msg.subject << ": " << resp.message;
    } catch (const std::exception& e) {
        ORES_LOG_SEV(requirement_coverage_handler_lg(), error)
            << msg.subject << " failed: " << e.what();
        resp.message = e.what();
    }
    reply(nats_, msg, resp);
}

}
