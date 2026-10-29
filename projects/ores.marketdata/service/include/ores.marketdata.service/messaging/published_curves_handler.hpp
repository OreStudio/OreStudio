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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_PUBLISHED_CURVES_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_PUBLISHED_CURVES_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"
#include "ores.marketdata.service/app/curve_withdrawal.hpp"
#include "ores.marketdata.service/app/published_curve_reader.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <exception>
#include <optional>

namespace ores::marketdata::messaging {

inline auto& published_curves_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.published_curves_handler");
    return instance;
}

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief The published curve screens' reads, and the act that ends a set.
 *
 * The two reads answer the caller's tenant, which is what the list screen is:
 * every set the tenant holds, with the state each is in. They read publish
 * records, so they ask for the permission the publish records name:
 * `marketdata::curve_publishes:read`. That is also the permission the decider of
 * a publish holds, which is what lets a queue row and a request page name the
 * subject the decision is about.
 *
 * The withdrawal is the maker's act and not the decider's: the account that may
 * publish a curve may take one out of use, as the journey states, so it asks for
 * the publish permission. A request that stands behind the act is read and
 * checked here, so a refusal names the reason rather than leaving the caller to
 * guess.
 */
class published_curves_handler {
public:
    published_curves_handler(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(published_curves_handler_lg(), msg);
        const auto req_ctx = context_of(msg, "marketdata::curve_publishes:read");
        if (!req_ctx)
            return;
        auto req = decode<list_published_curves_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(published_curves_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        list_published_curves_response resp;
        try {
            resp = service::app::list_published_curves(*req_ctx, *req);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(published_curves_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void sets(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(published_curves_handler_lg(), msg);
        const auto req_ctx = context_of(msg, "marketdata::curve_publishes:read");
        if (!req_ctx)
            return;
        auto req = decode<get_published_curve_sets_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(published_curves_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        get_published_curve_sets_response resp;
        try {
            resp = service::app::get_published_curve_sets(*req_ctx, *req);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(published_curves_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void withdraw(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(published_curves_handler_lg(), msg);
        // The withdrawal is the maker's act, and the maker is the permission the
        // withdrawal kind names.
        const auto req_ctx = context_of(
            msg, service::app::raise_permission_of(ctx_, service::app::curve_withdrawal_kind_code));
        if (!req_ctx)
            return;
        auto req = decode<withdraw_curve_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(published_curves_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        withdraw_curve_response resp;
        try {
            resp = service::app::withdraw_curve(*req_ctx, *req);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(published_curves_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    /**
     * @brief The caller's context, or nothing after a refusal has been sent.
     *
     * Every message of this handler asks the same two questions -- is the caller
     * established, and does it hold the permission the act needs -- so they are
     * asked once and answered the same way.
     */
    std::optional<ores::database::context> context_of(ores::nats::message& msg,
                                                      std::string_view permission) {
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return std::nullopt;
        }
        if (!permission.empty() && !has_permission(*req_ctx_expected, permission)) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return std::nullopt;
        }
        return std::optional<ores::database::context>{*req_ctx_expected};
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
