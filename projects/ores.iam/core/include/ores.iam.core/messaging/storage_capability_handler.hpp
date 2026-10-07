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
#ifndef ORES_IAM_CORE_MESSAGING_STORAGE_CAPABILITY_HANDLER_HPP
#define ORES_IAM_CORE_MESSAGING_STORAGE_CAPABILITY_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/messaging/storage_capability_operations_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_set>

namespace ores::iam::messaging {

namespace {
inline auto& storage_capability_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.iam.messaging.storage_capability_handler");
    return instance;
}
}

/**
 * @brief Hand-written NATS handler for minting a storage capability.
 *
 * The capability is a signed token whose claims name the buckets, key prefixes
 * and operations its holder may use. IAM signs it with the key it already
 * holds, so the storage service verifies it against the one published key and
 * asks no further question. The row type is the one the token carries, so the
 * caller and the verifier build and read the same shape.
 */
class storage_capability_handler {
public:
    storage_capability_handler(ores::nats::service::client& nats,
                               ores::database::context ctx,
                               std::optional<ores::security::jwt::jwt_authenticator> signer)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , signer_(std::move(signer)) {}

    /**
     * @brief Serves iam.v1.storage_capabilities.mint.
     */
    void mint(ores::nats::message msg) {
        using namespace ores::logging;
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::has_permission;
        using ores::service::messaging::log_handler_entry;
        using ores::service::messaging::reply;

        log_handler_entry(storage_capability_handler_lg(), msg);
        auto req_ctx = ores::service::service::make_request_context(ctx_, msg, signer_);
        if (!req_ctx) {
            error_reply(nats_, msg, req_ctx.error());
            return;
        }
        auto req = decode<mint_storage_capability_request>(msg);
        if (!req) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        mint_storage_capability_response resp;
        const auto refuse = [&](std::string reason) {
            resp.success = false;
            resp.message = std::move(reason);
            reply(nats_, msg, resp);
        };

        if (!has_permission(*req_ctx, mint_permission)) {
            BOOST_LOG_SEV(storage_capability_handler_lg(), warn)
                << "mint denied: the caller lacks " << mint_permission;
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        if (!signer_) {
            refuse("This deployment cannot issue a storage capability.");
            return;
        }
        const auto tenant = ores::utility::uuid::tenant_id::from_string(req->tenant_id);
        if (!tenant) {
            refuse("The request names no tenant: " + req->tenant_id + ".");
            return;
        }
        if (req->grants.empty()) {
            refuse("The request names no grants.");
            return;
        }
        for (const auto& grant : req->grants) {
            if (grant.bucket.empty() || grant.key_prefix.empty()) {
                refuse("A grant names no bucket or no key prefix.");
                return;
            }
            if (!allowed_ops().contains(grant.op)) {
                refuse("Unknown storage operation: " + grant.op + ".");
                return;
            }
        }

        const auto now = std::chrono::system_clock::now();
        ores::security::jwt::jwt_claims claims;
        claims.subject = req_ctx->actor();
        claims.tenant_id = tenant->to_string();
        claims.storage_grants = req->grants;
        claims.issued_at = now;
        claims.expires_at = now + capability_lifetime;

        const auto token = signer_->create_token(claims);
        if (!token) {
            refuse("The storage capability could not be signed.");
            return;
        }

        BOOST_LOG_SEV(storage_capability_handler_lg(), info)
            << "Minted a storage capability for tenant " << *claims.tenant_id << " with "
            << claims.storage_grants.size() << " grant(s)";

        resp.success = true;
        resp.token = *token;
        resp.expires_at =
            std::chrono::duration_cast<std::chrono::seconds>(claims.expires_at.time_since_epoch())
                .count();
        reply(nats_, msg, resp);
    }

private:
    static constexpr std::string_view mint_permission = "iam::storage_capabilities:mint";

    /**
     * @brief The lifetime of a capability: long enough for one assignment.
     */
    static constexpr std::chrono::hours capability_lifetime{1};

    static const std::unordered_set<std::string>& allowed_ops() {
        static const std::unordered_set<std::string> ops{"get", "put", "delete", "list"};
        return ops;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> signer_;
};

}

#endif
