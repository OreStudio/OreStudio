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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_PROVIDER_TERMS_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_PROVIDER_TERMS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/provider_terms_operations_protocol.hpp"
#include "ores.marketdata.core/repository/provider_repository.hpp"
#include "ores.marketdata.core/terms/provider_terms.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <chrono>
#include <optional>

namespace ores::marketdata::messaging {

namespace {

inline auto& provider_terms_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.provider_terms_handler");
    return instance;
}

}

/**
 * @brief Records the acceptance of a provider's terms, the one write that does.
 *
 * The caller names the provider. The terms accepted are the ones the service holds, the
 * account is the session's and the time is the service's clock, so no caller can state an
 * acceptance of terms it did not see.
 */
class provider_terms_handler {
public:
    provider_terms_handler(ores::nats::service::client& nats,
                           ores::database::context ctx,
                           std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void accept(ores::nats::message msg) {
        using namespace ores::logging;
        using ores::service::messaging::error_reply;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!ores::service::messaging::has_permission(req_ctx,
                                                      "marketdata::providers:accept_terms")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = ores::service::messaging::decode<accept_provider_terms_request>(msg);
        if (!req) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        accept_provider_terms_response resp;
        try {
            repository::provider_repository repo;
            auto found = repo.read_latest(req_ctx, req->provider_code);
            if (found.empty()) {
                resp.message = "No provider '" + req->provider_code + "'.";
            } else if (!terms::has_terms(found.front())) {
                resp.message = "The provider '" + req->provider_code + "' states no terms.";
            } else {
                const auto& actor = req_ctx.actor();
                auto accepted =
                    terms::accept_terms(found.front(), actor, std::chrono::system_clock::now());
                accepted.modified_by = actor;
                accepted.performed_by = req_ctx.service_account();
                accepted.change_reason_code = "system.update";
                accepted.change_commentary = "Accepted the terms at " + accepted.terms_url;
                repo.write(req_ctx, accepted);
                resp.success = true;
                resp.message = "Accepted the terms of '" + req->provider_code + "'.";
                resp.accepted_by = accepted.terms_accepted_by;
                resp.accepted_at = accepted.terms_accepted_at;
                BOOST_LOG_SEV(provider_terms_handler_lg(), info)
                    << actor << " accepted the terms of provider '" << req->provider_code << "'";
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(provider_terms_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        ores::service::messaging::reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
