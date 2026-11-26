/** -*- mode: c++-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
#ifndef ORES_DQ_CORE_MESSAGING_ROSTER_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_ROSTER_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/messaging/bundle_roster_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/service/publish_service.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/correlation.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>
#include <string>

namespace ores::dq::messaging {

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;

/**
 * @brief The roster verb: every bundle, and for one of them its members.
 *
 * The answer is not a row any table holds: it is the level a bundle publishes
 * at, the number of datasets it carries and, for the bundle a caller names, the
 * number of rows each member's artefact table has staged. That is why the
 * handler is hand-written beside the =bundle_roster= operation model that
 * declares its messages, and why the counts are read from the discovery
 * functions rather than assembled here.
 *
 * A request that names no bundle answers the roster alone. The staged count
 * costs one count per member, so it is taken only for the bundle named.
 */
class roster_handler {
public:
    roster_handler(ores::nats::service::client& nats,
                   const ores::database::context& ctx,
                   std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(std::move(verifier)) {}

    void bundle_roster(ores::nats::message msg) {
        ORES_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

        auto req = decode<bundle_roster_request>(msg);
        if (!req) {
            ORES_LOG_SEV(lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "dq::datasets:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        bundle_roster_response resp;
        try {
            service::publish_service svc(ctx);
            resp.bundles = svc.list_bundle_roster();

            if (!req->bundle_code.empty()) {
                auto members = svc.list_bundle_members(req->bundle_code);
                for (auto& bundle : resp.bundles) {
                    if (bundle.code == req->bundle_code) {
                        bundle.members = std::move(members);
                        break;
                    }
                }
            }
            resp.success = true;
        } catch (const std::exception& e) {
            ORES_LOG_SEV(lg(), error) << "Failed to read the roster: " << e.what();
            resp.success = false;
            resp.error_message = e.what();
        }

        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;

    /**
     * @brief The service's own context, held as a value and never as a reference.
     *
     * Registration receives the context as a temporary, so a reference would
     * dangle as soon as registration returns and every request would then read
     * a freed implementation. A copy costs one shared pointer increment.
     */
    ores::database::context ctx_;

    std::optional<ores::security::jwt::jwt_authenticator> verifier_;

    static ores::logging::logger_t& lg() {
        static auto instance = ores::logging::make_logger("ores.dq.messaging.roster_handler");
        return instance;
    }
};

} // namespace ores::dq::messaging

#endif
