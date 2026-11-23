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
#ifndef ORES_IAM_CORE_MESSAGING_AGENT_GRANT_OPERATIONS_HANDLER_HPP
#define ORES_IAM_CORE_MESSAGING_AGENT_GRANT_OPERATIONS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/messaging/agent_grant_operations_protocol.hpp"
#include "ores.iam.core/service/agent_grant_operations_service.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>
#include <string_view>

namespace ores::iam::messaging {

/**
 * @brief Hand-written NATS handler for LLM agent grants and keys.
 *
 * The agent grant and key protocols carry their reads only, so these
 * operations are the tables' only writes. The handler proves the request and
 * replies; the service makes every check. The exchange is the one operation
 * that mints a token, so it is the one that hands the service the signer.
 */
class agent_grant_operations_handler {
private:
    inline static std::string_view logger_name =
        "ores.iam.messaging.agent_grant_operations_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    agent_grant_operations_handler(ores::nats::service::client& nats,
                                   const ores::database::context& ctx,
                                   std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(verifier) {}

    void create_grant(ores::nats::message msg) {
        handle<create_agent_grant_request>(
            std::move(msg), [](auto& svc, const auto& req) { return svc.create_agent_grant(req); });
    }

    void create_key(ores::nats::message msg) {
        handle<create_agent_key_request>(
            std::move(msg), [](auto& svc, const auto& req) { return svc.create_agent_key(req); });
    }

    void revoke_grant(ores::nats::message msg) {
        handle<revoke_agent_grant_request>(
            std::move(msg), [](auto& svc, const auto& req) { return svc.revoke_agent_grant(req); });
    }

    void revoke_key(ores::nats::message msg) {
        handle<revoke_agent_key_request>(
            std::move(msg), [](auto& svc, const auto& req) { return svc.revoke_agent_key(req); });
    }

    void list_activity(ores::nats::message msg) {
        handle<list_own_agent_activity_request>(std::move(msg), [](auto& svc, const auto& req) {
            return svc.list_own_agent_activity(req);
        });
    }

    void list_every(ores::nats::message msg) {
        handle<list_every_agent_grant_request>(std::move(msg), [](auto& svc, const auto& req) {
            return svc.list_every_agent_grant(req);
        });
    }

    void list_own(ores::nats::message msg) {
        handle<list_own_agent_grants_request>(std::move(msg), [](auto& svc, const auto& req) {
            return svc.list_own_agent_grants(req);
        });
    }

    void exchange(ores::nats::message msg) {
        handle<exchange_agent_key_request>(
            std::move(msg),
            [](auto& svc, const auto& req) { return svc.exchange_agent_key(req); },
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

        log_handler_entry(lg(), msg);
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
            service::agent_grant_operations_service svc(*req_ctx, std::move(signer));
            reply(nats_, msg, call(svc, *req));
        } catch (const std::exception& e) {
            // The message is logged, never the request: an exchange carries a key.
            ORES_LOG_SEV(lg(), error) << msg.subject << " failed: " << e.what();
            typename Request::response_type failure;
            failure.message = "The request failed.";
            reply(nats_, msg, failure);
        }
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
