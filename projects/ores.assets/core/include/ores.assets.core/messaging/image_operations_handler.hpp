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
#ifndef ORES_ASSETS_CORE_MESSAGING_IMAGE_OPERATIONS_HANDLER_HPP
#define ORES_ASSETS_CORE_MESSAGING_IMAGE_OPERATIONS_HANDLER_HPP

#include "ores.assets.api/messaging/image_operations_protocol.hpp"
#include "ores.assets.core/service/image_operations_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <exception>
#include <optional>
#include <utility>

namespace ores::assets::messaging {

namespace {

inline auto& image_operations_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.assets.messaging.image_operations_handler");
    return instance;
}

}

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief NATS message handler for the image operations.
 *
 * The adapter decides nothing: it proves the request, decodes the canonical
 * request, calls the service and replies with the response the service
 * filled. The upload carries no permission check, because any signed-in
 * account may upload: the image lands in the caller's tenant, and the
 * account a photo appears on is decided by the self write that references
 * the returned id.
 */
class image_operations_handler {
public:
    image_operations_handler(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves assets.v1.images.upload.
     */
    void upload_image(ores::nats::message msg) {
        BOOST_LOG_SEV(image_operations_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<upload_image_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(image_operations_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::image_operations_service svc(req_ctx);
        try {
            auto response = svc.upload_image(*req);
            BOOST_LOG_SEV(image_operations_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different thing
            // and is reported as such. The store's words stay in the log.
            BOOST_LOG_SEV(image_operations_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            upload_image_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = "The upload failed.";
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves assets.v1.images.upload-policy.
     */
    void get_image_upload_policy(ores::nats::message msg) {
        BOOST_LOG_SEV(image_operations_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!ores::service::messaging::has_permission(req_ctx, "assets::images:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_image_upload_policy_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(image_operations_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::image_operations_service svc(req_ctx);
        try {
            auto response = svc.get_image_upload_policy(*req);
            BOOST_LOG_SEV(image_operations_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(image_operations_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_image_upload_policy_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = "The read failed.";
            reply(nats_, msg, failure);
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
