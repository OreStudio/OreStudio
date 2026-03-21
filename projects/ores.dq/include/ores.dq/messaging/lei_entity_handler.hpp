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
#ifndef ORES_DQ_MESSAGING_LEI_ENTITY_HANDLER_HPP
#define ORES_DQ_MESSAGING_LEI_ENTITY_HANDLER_HPP

#include <optional>
#include <rfl/json.hpp>
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.dq/messaging/lei_entity_summary_protocol.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

namespace {
inline auto& lei_entity_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.lei_entity_handler");
    return instance;
}
} // namespace

class lei_entity_handler {
public:
    lei_entity_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void summary(ores::nats::message msg) {
        BOOST_LOG_SEV(lei_entity_handler_lg(), debug) << "Handling " << msg.subject;
        const std::string_view data(
            reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
        const auto parsed = rfl::json::read<get_lei_entities_summary_request>(data);
        if (!parsed) {
            const auto err = parsed.error().what();
            BOOST_LOG_SEV(lei_entity_handler_lg(), error)
                << "Failed to decode " << msg.subject
                << ": " << err
                << " (payload: " << data << ")";
            get_lei_entities_summary_response err_resp;
            err_resp.success = false;
            err_resp.error_message = std::string("Failed to decode request: ") + err;
            reply(nats_, msg, err_resp);
            return;
        }
        const auto& req = *parsed;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        get_lei_entities_summary_response resp;
        try {
            using namespace ores::database::repository;
            if (req.country_filter.empty()) {
                const std::string sql =
                    "SELECT * FROM ores_dq_lei_entities_distinct_countries_fn()";
                auto rows = execute_raw_multi_column_query(
                    ctx, sql, lei_entity_handler_lg(),
                    "listing LEI countries");
                for (const auto& row : rows) {
                    if (!row.empty() && row[0])
                        resp.entities.push_back({.country = *row[0]});
                }
            } else {
                const std::string sql =
                    "SELECT * FROM ores_dq_lei_entities_summary_by_country_fn($1, $2, $3)";
                auto rows = execute_parameterized_multi_column_query(
                    ctx, sql,
                    {req.country_filter,
                     std::to_string(req.limit),
                     std::to_string(req.offset)},
                    lei_entity_handler_lg(),
                    "listing LEI entities by country");
                for (const auto& row : rows) {
                    if (row.size() >= 4)
                        resp.entities.push_back({
                            .lei = row[0].value_or(""),
                            .entity_legal_name = row[1].value_or(""),
                            .entity_category = row[2].value_or(""),
                            .country = row[3].value_or("")});
                }
            }
            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lei_entity_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.error_message = e.what();
        }
        BOOST_LOG_SEV(lei_entity_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

private:

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::dq::messaging

#endif
