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
#ifndef ORES_SYNTHETIC_SERVICE_SIMULATE_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_SIMULATE_HANDLER_HPP

#include "feed_kind_registry.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>
#include <string>

namespace ores::synthetic::service {

namespace {
inline auto& simulate_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.simulate_handler");
    return instance;
}
} // namespace

using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief Stateless batch simulation of sample paths, one subject per asset
 * class.
 *
 * The envelope is kind-neutral: the registry holds this kind's subject, its
 * config permission, and the two closures that decode its own typed request and
 * reply with its own typed response, while run_simulate_paths owns the clamp,
 * the per-path seeding and the stepping. Nothing is persisted, published or
 * streamed: it is a dry run so a caller can preview the configured behaviour
 * before starting a feed.
 */
class simulate_handler {
public:
    simulate_handler(ores::nats::service::client& nats,
                     ores::database::context ctx,
                     std::optional<ores::security::jwt::jwt_authenticator> verifier,
                     const feed_kind_registry& registry)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , registry_(registry) {}

    void simulate(ores::nats::message msg, const std::string& kind) {
        const auto* e = registry_.find(kind);
        if (!e) {
            // No registered kind, so no typed response of its own to reply
            // with: the registry answers in its own registered shape.
            BOOST_LOG_SEV(simulate_handler_lg(), error) << "Unknown feed kind: " << kind;
            registry_.reply_unknown_kind(nats_, msg, kind);
            return;
        }

        // Standard service auth: require a valid JWT, then RBAC.
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            BOOST_LOG_SEV(simulate_handler_lg(), warn)
                << "Rejecting simulate request: auth failed: "
                << static_cast<int>(ctx_expected.error());
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        if (!has_permission(*ctx_expected, e->config_permission)) {
            BOOST_LOG_SEV(simulate_handler_lg(), warn)
                << "Rejecting simulate request: missing permission " << e->config_permission << ".";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto env = e->decode_simulate(msg);
        if (!env) {
            BOOST_LOG_SEV(simulate_handler_lg(), error) << "Failed to decode simulate request.";
            e->reply_simulate(
                nats_,
                msg,
                feed_simulation_result{.success = false,
                                       .message = "Failed to decode simulate request."});
            return;
        }

        e->reply_simulate(nats_, msg, run_simulate_paths(*env));
        BOOST_LOG_SEV(simulate_handler_lg(), debug) << "Reply sent for " << kind << ".simulate.";
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    const feed_kind_registry& registry_;
};

}

#endif
