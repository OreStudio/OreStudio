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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_APPROVAL_GATED_CURVE_PUBLISH_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_APPROVAL_GATED_CURVE_PUBLISH_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/curve_publish_operations_protocol.hpp"
#include "ores.marketdata.service/app/curve_notices.hpp"
#include "ores.marketdata.service/app/curve_publish_gate.hpp"
#include "ores.marketdata.service/app/curve_republish_service.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <exception>
#include <optional>
#include <string>

namespace ores::marketdata::messaging {

namespace {
inline auto& curve_publish_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.marketdata.messaging.approval_gated_curve_publish_handler");
    return instance;
}
}

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief The approval-gated publish of a curve.
 *
 * A publish is two calls. The first carries no approved request: it raises the
 * @c marketdata.curve_publish request for the signed-in person and answers
 * waiting, so the publish waits on the decision. The second names the request
 * the decider approved, and only then does the published set move: the handler
 * asks the record whether the request permits the publish before it calls
 * @c curve_republish_service::republish(), so a refusal leaves the curve as it
 * was.
 *
 * The permission is the maker's, and it is the one the kind names in
 * =raise_permission_code=: the pair is per kind, so the handler asks the kind and
 * never a code of its own. The decision is the decider's, and it is taken on the
 * inbox's own operation, so the two halves ask for two different codes.
 */
class approval_gated_curve_publish_handler {
public:
    approval_gated_curve_publish_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void publish(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(curve_publish_handler_lg(), msg);
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        // The maker's permission is the kind's, so the gate moves when the tenant
        // moves the kind and never because a code here went stale.
        std::string maker_permission;
        try {
            maker_permission =
                service::app::raise_permission_of(req_ctx, service::app::curve_publish_kind_code);
        } catch (const std::exception& e) {
            // A kind that cannot be read names no maker, so the gate fails closed
            // rather than letting a publish through an unstated rule.
            BOOST_LOG_SEV(curve_publish_handler_lg(), error)
                << msg.subject << " refused: " << e.what();
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        if (!maker_permission.empty() && !has_permission(req_ctx, maker_permission)) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<publish_curve_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(curve_publish_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        publish_curve_response resp;
        try {
            answer(req_ctx, *req, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(curve_publish_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        BOOST_LOG_SEV(curve_publish_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

private:
    /**
     * @brief Raises the request a publish waits on, or applies an approved one.
     *
     * Both halves read the subject out of the configuration the call names, so
     * the party and the series are the configuration's and never the caller's.
     *
     * A kind whose @c approvals_required is zero needs nobody, so the raise is
     * the publish: the inbox closes the request approved in the same act and
     * this operation applies it at once, answering with the instant and the
     * publisher the record holds rather than with a wait. The publisher is the
     * asker, because the asker is the person who applied it.
     */
    static void answer(const ores::database::context& req_ctx,
                       const publish_curve_request& req,
                       publish_curve_response& resp) {
        using namespace ores::marketdata::service::app;

        if (req.approval_request_id.empty()) {
            const auto subject =
                resolve_publish_subject(req_ctx, req.bootstrap_config_id, req.as_of);
            const auto raised = raise_publish_request(req_ctx, subject, req.reason);
            const auto id = boost::uuids::to_string(raised.id);
            resp.approval_request_id = id;
            resp.state_code = raised.state_code;
            if (raised.state_code != "approved") {
                resp.success = true;
                resp.message = "Raised. The publish waits on the decision.";
                BOOST_LOG_SEV(curve_publish_handler_lg(), info)
                    << "Raised publish request " << id << " for curve " << req.bootstrap_config_id
                    << " as of " << req.as_of;
                return;
            }
            BOOST_LOG_SEV(curve_publish_handler_lg(), info)
                << "Request " << id << " needs no approval, so the raise is the publish";
            apply(req_ctx, id, subject, resp);
            return;
        }

        const auto attempted = resolve_publish_subject(req_ctx, req.bootstrap_config_id, req.as_of);
        resp.approval_request_id = req.approval_request_id;
        apply(req_ctx, req.approval_request_id, attempted, resp);
    }

    /**
     * @brief Applies the approved request, or says why it may not.
     *
     * The set moves before the record is stamped, so a failure to write the
     * observations leaves the request unused rather than spent.
     */
    static void apply(const ores::database::context& req_ctx,
                      const std::string& request_id,
                      const service::app::publish_subject& attempted,
                      publish_curve_response& resp) {
        using namespace ores::marketdata::service::app;

        const auto verdict = verdict_on_publish(req_ctx, request_id, attempted);
        resp.state_code = verdict.state_code;
        if (!verdict.approved) {
            resp.message = verdict.message;
            return;
        }

        curve_republish_service::republish(req_ctx, attempted.bootstrap_config_id, attempted.as_of);
        const auto published = record_publication(
            req_ctx,
            *verdict.record,
            req_ctx.actor().empty() ? req_ctx.service_account() : req_ctx.actor());
        resp.success = true;
        resp.published_at = published.published_at;
        resp.published_by = published.published_by;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
