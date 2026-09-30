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
#ifndef ORES_DQ_CORE_MESSAGING_LEI_ENTITY_SUMMARY_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_LEI_ENTITY_SUMMARY_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq.api/messaging/lei_entity_summary_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>
#include <rfl/json.hpp>

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::error_reply;
using namespace ores::logging;

namespace {
inline auto& dq_lei_entity_summary_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.dq.messaging.lei_entity_summary_handler");
    return instance;
}
} // namespace

/**
 * @brief Serves dq.v1.lei-entities.summary and dq.v1.lei-entities.search.
 *
 * The operation model generates no handler, so this handler is hand-written
 * beside it, as report_definition_template_handler.hpp is. Every read goes
 * through a SQL function: the summary reads list the distinct countries or the
 * active root entities of one country, which is the country browser the shell
 * offers, and the search matches a legal name or an LEI across countries and
 * states how many parties the match's hierarchy would create.
 */
class lei_entity_summary_handler {
public:
    lei_entity_summary_handler(ores::nats::service::client& nats,
                               ores::database::context ctx,
                               std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void summary(ores::nats::message msg) {
        BOOST_LOG_SEV(dq_lei_entity_summary_handler_lg(), debug) << "Handling " << msg.subject;
        /*
         * The payload is read with the deployment's own wire codec, not as
         * JSON: a caller reaches this subject through the platform's transport,
         * which encodes in the format the deployment states, and a handler that
         * insisted on one format refused every caller that used another.
         */
        const auto parsed = ores::service::messaging::decode<get_lei_entities_summary_request>(msg);
        if (!parsed) {
            get_lei_entities_summary_response err_resp;
            err_resp.success = false;
            err_resp.error_message = "The request could not be read.";
            reply(nats_, msg, err_resp);
            return;
        }
        const auto& req = *parsed;
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
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
                    ctx, sql, dq_lei_entity_summary_handler_lg(), "listing LEI countries");
                for (const auto& row : rows) {
                    if (!row.empty() && row[0])
                        resp.entities.push_back({.country = *row[0]});
                }
            } else {
                const std::string sql =
                    "SELECT * FROM ores_dq_lei_entities_summary_by_country_fn($1, $2, $3)";
                auto rows = execute_parameterized_multi_column_query(
                    ctx,
                    sql,
                    {req.country_filter, std::to_string(req.limit), std::to_string(req.offset)},
                    dq_lei_entity_summary_handler_lg(),
                    "listing LEI entities by country");
                for (const auto& row : rows) {
                    if (row.size() >= 4)
                        resp.entities.push_back({.lei = row[0].value_or(""),
                                                 .entity_legal_name = row[1].value_or(""),
                                                 .entity_category = row[2].value_or(""),
                                                 .country = row[3].value_or("")});
                }
            }
            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dq_lei_entity_summary_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.error_message = e.what();
        }
        BOOST_LOG_SEV(dq_lei_entity_summary_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void search(ores::nats::message msg) {
        BOOST_LOG_SEV(dq_lei_entity_summary_handler_lg(), debug) << "Handling " << msg.subject;
        const auto parsed = ores::service::messaging::decode<search_lei_entities_request>(msg);
        if (!parsed) {
            search_lei_entities_response err_resp;
            err_resp.success = false;
            err_resp.error_message = "The request could not be read.";
            reply(nats_, msg, err_resp);
            return;
        }
        const auto& req = *parsed;
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        search_lei_entities_response resp;
        try {
            using namespace ores::database::repository;
            const std::string sql = "SELECT * FROM ores_dq_lei_entities_search_fn($1, $2, $3, $4)";
            auto rows = execute_parameterized_multi_column_query(ctx,
                                                                 sql,
                                                                 {req.search,
                                                                  req.country_filter,
                                                                  std::to_string(req.limit),
                                                                  std::to_string(req.offset)},
                                                                 dq_lei_entity_summary_handler_lg(),
                                                                 "searching LEI entities");
            for (const auto& row : rows) {
                if (row.size() >= 5)
                    resp.entities.push_back({.lei = row[0].value_or(""),
                                             .entity_legal_name = row[1].value_or(""),
                                             .entity_category = row[2].value_or(""),
                                             .country = row[3].value_or(""),
                                             .party_count = std::stoll(row[4].value_or("0"))});
            }
            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dq_lei_entity_summary_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.error_message = e.what();
        }
        BOOST_LOG_SEV(dq_lei_entity_summary_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::dq::messaging

#endif
