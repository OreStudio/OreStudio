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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_nats_handler.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_CORE_MESSAGING_TENOR_CONVENTION_RESOLUTION_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_TENOR_CONVENTION_RESOLUTION_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/tenor_convention_resolution_protocol.hpp"
#include "ores.refdata.core/service/tenor_convention_resolution_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::refdata::messaging {

namespace {
inline auto& tenor_convention_resolution_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.refdata.messaging.tenor_convention_resolution_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for tenor convention resolutions operations.
 *
 * The adapter decides nothing: it proves the request, checks the permission a
 * write needs, decodes the canonical request, calls the service and replies
 * with the response the service filled. The outcome a caller reads -- missing,
 * conflicting, denied -- is the service's answer, so the two cannot disagree
 * about what happened.
 */
class tenor_convention_resolution_handler {
public:
    tenor_convention_resolution_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves refdata.v1.tenor_convention_resolutions.list.
     */
    void list_tenor_convention_resolutions(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<list_tenor_convention_resolutions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_convention_resolution_service svc(req_ctx);
        try {
            auto response = svc.list_tenor_convention_resolutions(*req);
            BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_tenor_convention_resolutions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.tenor_convention_resolutions.get.
     */
    void get_tenor_convention_resolution(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<get_tenor_convention_resolution_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_convention_resolution_service svc(req_ctx);
        try {
            auto response = svc.get_tenor_convention_resolution(*req);
            BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_tenor_convention_resolution_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.tenor_convention_resolutions.get_many.
     */
    void get_many_tenor_convention_resolutions(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<get_many_tenor_convention_resolutions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_convention_resolution_service svc(req_ctx);
        try {
            auto response = svc.get_many_tenor_convention_resolutions(*req);
            BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_many_tenor_convention_resolutions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.tenor_convention_resolutions.list_by_convention_code.
     */
    void list_by_convention_code_tenor_convention_resolutions(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<list_by_convention_code_tenor_convention_resolutions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_convention_resolution_service svc(req_ctx);
        try {
            auto response = svc.list_by_convention_code_tenor_convention_resolutions(*req);
            BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_convention_resolution_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_by_convention_code_tenor_convention_resolutions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::refdata::messaging

#endif
