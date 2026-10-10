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
#ifndef ORES_IAM_MESSAGING_AUTH_EVENT_HANDLER_HPP
#define ORES_IAM_MESSAGING_AUTH_EVENT_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/messaging/auth_event_operations_protocol.hpp"
#include "ores.iam.core/repository/auth_event_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"

namespace ores::iam::messaging {

namespace {

inline auto& auth_event_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.iam.messaging.auth_event_handler");
    return instance;
}

} // namespace

using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief Serves the authentication event log.
 *
 * The log is the record of what happened, so the handler reads it and
 * writes nothing. The table carries no row-level security, so the read
 * scopes on the caller's tenant itself.
 */
class auth_event_handler {
public:
    auth_event_handler(ores::nats::service::client& nats,
                       ores::database::context ctx,
                       ores::security::jwt::jwt_authenticator signer)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , signer_(std::move(signer)) {}

    /**
     * @brief Serves iam.v1.auth_events.list.
     */
    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(auth_event_handler_lg(), msg);
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, signer_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "iam::auth_events:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto request = ores::service::messaging::decode<list_auth_events_request>(msg);
        if (!request) {
            BOOST_LOG_SEV(auth_event_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        try {
            repository::auth_event_repository repo(req_ctx);
            const auto rows = repo.read_events(req_ctx,
                                               req_ctx.tenant_id().to_string(),
                                               request->account_id,
                                               request->event_type,
                                               request->from_time,
                                               request->to_time,
                                               request->limit,
                                               request->offset);

            std::vector<auth_event> events;
            events.reserve(rows.size());
            for (const auto& row : rows) {
                events.push_back(auth_event{.id = row.id.value(),
                                            .event_time = row.event_time.value(),
                                            .account_id = row.account_id,
                                            .event_type = row.event_type,
                                            .username = row.username,
                                            .session_id = row.session_id,
                                            .party_id = row.party_id,
                                            .error_detail = row.error_detail});
            }

            BOOST_LOG_SEV(auth_event_handler_lg(), debug)
                << "Completed " << msg.subject << " with " << events.size() << " event(s)";
            reply(nats_,
                  msg,
                  list_auth_events_response{.events = std::move(events), .success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auth_event_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, list_auth_events_response{.success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
