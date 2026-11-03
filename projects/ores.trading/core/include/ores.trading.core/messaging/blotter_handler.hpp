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
#ifndef ORES_TRADING_CORE_MESSAGING_BLOTTER_HANDLER_HPP
#define ORES_TRADING_CORE_MESSAGING_BLOTTER_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.trading.api/messaging/blotter_protocol.hpp"
#include "ores.trading.core/repository/trade_summary_repository.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <array>
#include <cstdint>
#include <exception>
#include <stdexcept>
#include <optional>
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
inline constexpr std::array<std::string_view, 5> blotter_dimensions = {
    "portfolio", "book", "counterparty", "asset_class", "status"};

}

/**
 * @brief Hand-written NATS handler for the blotter's reads.
 *
 * Lives outside the generated handlers because the blotter asks two questions
 * no entity verb answers: one page of rows that starts after a key, and the
 * counts at each node of a rail. The counts are grouped by the store, so the
 * answer does not depend on how large the book has grown.
 */
class blotter_handler {
public:
    blotter_handler(ores::nats::service::client& nats,
                    ores::database::context ctx,
                    std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves trading.v1.blotter.rail.
     */
    void get_blotter_rail(ores::nats::message msg) {
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
                     * Every row is counted by every rail, so the state rail's
                     * counts are the row total. A book with no booking is not in
                     * the book rail, and the party's figure must not lose it.
                     */
                    if (dimension == "status") {
                        party_total += count;
                    }
                }
            }
            response.nodes.insert(response.nodes.begin(),
                                  blotter_rail_node{
                                      .dimension = "party", .node_id = "", .count = party_total});
            response.result.outcome = ores::utility::domain::outcome::ok;
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

    /**
     * @brief Serves trading.v1.blotter.page.
     */
    void get_blotter_page(ores::nats::message msg) {
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
        auto req = decode<get_blotter_page_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(blotter_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        /*
         * A limit of zero would ask the store for no rows and answer as though
         * the book ended, so the page states a size of its own.
         */
        constexpr std::uint32_t default_limit = 100;
        constexpr std::uint32_t largest_limit = 1000;
        const auto asked = req->limit == 0 ? default_limit : std::min(req->limit, largest_limit);

        get_blotter_page_response response;
        try {
            repository::trade_summary_repository summaries;
            const auto rows = summaries.read_blotter_page(
                req_ctx, asked, req->order, req->after, req->filter);
            response.rows = rows;
            /*
             * A short page is the last one, but a full page that happens to end
             * the book is not: the caller is told where the next page starts
             * only when one may exist, and the store answers the rest.
             */
            if (rows.size() == asked)
                response.next =
                    repository::trade_summary_repository::blotter_page_key(rows.back(),
                                                                           req->order.field);
            response.result.outcome = ores::utility::domain::outcome::ok;
            BOOST_LOG_SEV(blotter_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::invalid_argument& e) {
            /*
             * A key the service did not write, or an order it cannot sort by, is
             * the caller's mistake and not the store's: it is answered as a bad
             * request rather than as a fault.
             */
            BOOST_LOG_SEV(blotter_handler_lg(), warn) << msg.subject << ": " << e.what();
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(blotter_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_blotter_page_response failure;
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
