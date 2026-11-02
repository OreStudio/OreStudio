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
#include "ores.trading.core/messaging/blotter_handler.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.trading.api/messaging/blotter_protocol.hpp"
#include "ores.trading.core/repository/trade_summary_repository.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <array>
#include <exception>
#include <string>
#include <string_view>
#include <utility>

namespace ores::trading::messaging {

namespace {
inline auto& blotter_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.trading.messaging.blotter_handler");
    return instance;
}

/*
 * The rails the screen draws, in the order it draws them. The store names a
 * grouping per dimension and refuses one it does not know, so the list is
 * stated here and not by the caller.
 */
constexpr std::array<std::string_view, 5> blotter_dimensions = {
    "portfolio", "book", "counterparty", "asset_class", "status"};
}

void blotter_handler::get_blotter_rail(ores::nats::message msg) {
    using ores::service::messaging::decode;
    using ores::service::messaging::error_reply;
    using ores::service::messaging::has_permission;
    using ores::service::messaging::reply;
    using namespace ores::logging;

    BOOST_LOG_SEV(blotter_handler_lg(), debug) << "Handling " << msg.subject;
    auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
    if (!req_ctx_expected) {
        error_reply(nats_, msg, req_ctx_expected.error());
        return;
    }
    const auto& req_ctx = *req_ctx_expected;
    if (!has_permission(req_ctx, "trading::trade_summaries:read")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    auto req = decode<get_blotter_rail_request>(msg);
    if (!req) {
        BOOST_LOG_SEV(blotter_handler_lg(), warn) << "Failed to decode: " << msg.subject;
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }

    get_blotter_rail_response response;
    try {
        repository::trade_summary_repository summaries;
        std::uint64_t party_total = 0;
        for (const auto dimension : blotter_dimensions) {
            const auto counts =
                summaries.count_blotter_rail(req_ctx, std::string(dimension), req->filter);
            for (const auto& [node_id, count] : counts) {
                response.nodes.push_back(blotter_rail_node{
                    .dimension = std::string(dimension), .node_id = node_id, .count = count});
                /*
                 * Every row is counted by every rail, so the state rail's counts
                 * are the row total; a book with no booking is not in the book
                 * rail, and the party's figure must not lose it.
                 */
                if (dimension == "status") {
                    party_total += count;
                }
            }
        }
        response.nodes.insert(response.nodes.begin(),
                              blotter_rail_node{
                                  .dimension = "party", .node_id = "", .count = party_total});
        response.result.outcome = ores::utility::domain::outcome::succeeded;
        BOOST_LOG_SEV(blotter_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, response);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(blotter_handler_lg(), error) << msg.subject << " failed: " << e.what();
        get_blotter_rail_response failure;
        failure.result.outcome = ores::utility::domain::outcome::failed;
        failure.result.code = "internal_error";
        failure.result.message = e.what();
        reply(nats_, msg, failure);
    }
}

}
