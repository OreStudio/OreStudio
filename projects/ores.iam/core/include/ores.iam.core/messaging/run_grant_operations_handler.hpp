/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_IAM_CORE_MESSAGING_RUN_GRANT_OPERATIONS_HANDLER_HPP
#define ORES_IAM_CORE_MESSAGING_RUN_GRANT_OPERATIONS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/messaging/run_grant_operations_protocol.hpp"
#include "ores.iam.core/service/run_grant_operations_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::iam::messaging {

namespace {
inline auto& run_grant_operations_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.iam.messaging.run_grant_operations_handler");
    return instance;
}
}

/**
 * @brief Hand-written NATS handler for creating, revoking and exchanging run
 * grants.
 *
 * The run grant's generated protocol carries its reads only, so these three
 * operations are the table's only writes. The handler proves the request and
 * replies; the service makes the checks, from the request context the token
 * produced. The exchange is the one operation that mints a token, so it is the
 * one that hands the service the authenticator it holds.
 */
class run_grant_operations_handler {
public:
    run_grant_operations_handler(ores::nats::service::client& nats,
                                 ores::database::context ctx,
                                 std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves iam.v1.run_grants.create.
     */
    void create(ores::nats::message msg) {
        handle<create_run_grant_request>(
            std::move(msg), [](auto& svc, const auto& req) { return svc.create_run_grant(req); });
    }

    /**
     * @brief Serves iam.v1.run_grants.revoke.
     */
    void revoke(ores::nats::message msg) {
        handle<revoke_run_grant_request>(
            std::move(msg), [](auto& svc, const auto& req) { return svc.revoke_run_grant(req); });
    }

    /**
     * @brief Serves iam.v1.run_grants.exchange.
     */
    void exchange(ores::nats::message msg) {
        handle<exchange_run_grant_request>(
            std::move(msg),
            [](auto& svc, const auto& req) { return svc.exchange_run_grant(req); },
            verifier_);
    }

private:
    template <typename Request, typename Call>
    void handle(ores::nats::message msg,
                Call call,
                std::optional<ores::security::jwt::jwt_authenticator> signer = std::nullopt) {
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::log_handler_entry;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        log_handler_entry(run_grant_operations_handler_lg(), msg);
        auto req_ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx) {
            error_reply(nats_, msg, req_ctx.error());
            return;
        }
        auto req = decode<Request>(msg);
        if (!req) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        try {
            service::run_grant_operations_service svc(*req_ctx, std::move(signer));
            reply(nats_, msg, call(svc, *req));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(run_grant_operations_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            typename Request::response_type failure;
            failure.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
