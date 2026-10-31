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
#ifndef ORES_REPORTING_SERVICE_MESSAGING_CUT_TAKE_HANDLER_HPP
#define ORES_REPORTING_SERVICE_MESSAGING_CUT_TAKE_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.service/service/cut_take.hpp"
#include "ores.reporting.core/service/nats_cut_body_reader.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <optional>
#include <string>
#include <utility>

namespace ores::reporting::messaging {

/**
 * @brief Serves reporting.v1.ops.take_cut: the hand take and its authorisation.
 *
 * The operation acts as the signed-in person: the session's party is the party
 * the body is fixed for, and the permission that guards the act is
 * =reporting::cuts:take=. The person's own Authorization header is forwarded to
 * the reads the assembly makes, so the population and the sets a person sees are
 * the ones their session may see.
 */
class cut_take_handler {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.reporting.messaging.cut_take_handler");
        return instance;
    }

public:
    cut_take_handler(ores::nats::service::client& nats,
                     ores::database::context ctx,
                     std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves reporting.v1.ops.take_cut.
     *
     * Assembling and taking a cut is one permission, because the assembly is
     * the first half of the take: a person who may not fix a body may not
     * preview one either.
     */
    void take_cut(ores::nats::message msg) {
        using ores::utility::domain::outcome;
        BOOST_LOG_SEV(lg(), ores::logging::debug) << "Handling " << msg.subject;

        auto req_ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx) {
            ores::service::messaging::error_reply(nats_, msg, req_ctx.error());
            return;
        }
        auto req = ores::service::messaging::decode<take_cut_request>(msg);
        if (!req) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        if (!ores::service::messaging::has_permission(*req_ctx, "reporting::cuts:take")) {
            ores::service::messaging::error_reply(
                nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        try {
            const auto authorization = bearer_of(msg);
            ores::reporting::service::nats_cut_body_reader reader(*req_ctx, nats_, authorization);
            auto answer = ores::reporting::service::cut_take_service(*req_ctx, reader).take(*req);
            BOOST_LOG_SEV(lg(), ores::logging::debug) << "Completed " << msg.subject;
            ores::service::messaging::reply(nats_, msg, answer);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << msg.subject << " failed: " << e.what();
            ores::service::messaging::reply(
                nats_,
                msg,
                take_cut_response{.result = {.outcome = outcome::failed,
                                             .code = "take_failed",
                                             .message = "The cut could not be taken.",
                                             .fields = {}}});
        }
    }

private:
    /**
     * @brief The caller's Authorization header, as the message carries it.
     *
     * The reads of the other components are made as the person, so the header
     * travels unchanged and never as the service's own grant.
     */
    std::string bearer_of(const ores::nats::message& msg) const {
        const auto it = msg.headers.find(std::string(ores::nats::headers::authorization));
        return it == msg.headers.end() ? std::string{} : it->second;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
