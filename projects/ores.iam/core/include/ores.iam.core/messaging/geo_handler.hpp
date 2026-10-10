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
#ifndef ORES_IAM_MESSAGING_GEO_HANDLER_HPP
#define ORES_IAM_MESSAGING_GEO_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.geo/service/geolocation_service.hpp"
#include "ores.iam.api/messaging/geo_operations_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"

namespace ores::iam::messaging {

namespace {

inline auto& geo_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.iam.messaging.geo_handler");
    return instance;
}

} // namespace

using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief Serves the address-to-country lookup.
 *
 * The ranges are published per tenant, so the lookup reads the caller's
 * tenant's ranges and no other's. An address they do not cover is not found,
 * which is an answer rather than an error.
 */
class geo_handler {
public:
    geo_handler(ores::nats::service::client& nats,
                ores::database::context ctx,
                ores::security::jwt::jwt_authenticator signer)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , signer_(std::move(signer)) {}

    /**
     * @brief Serves iam.v1.ops.lookup_country.
     */
    void lookup_country(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(geo_handler_lg(), msg);
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, signer_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "iam::geo:lookup")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto request = ores::service::messaging::decode<lookup_country_request>(msg);
        if (!request) {
            BOOST_LOG_SEV(geo_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        try {
            geo::service::geolocation_service geo(req_ctx);
            const auto located = geo.lookup(request->address);
            if (!located) {
                reply(nats_,
                      msg,
                      lookup_country_response{.found = false,
                                              .success = true,
                                              .message = "Address not covered by the tenant's "
                                                         "ranges"});
                return;
            }
            reply(nats_,
                  msg,
                  lookup_country_response{
                      .country_code = located->country_code, .found = true, .success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(geo_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, lookup_country_response{.success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
