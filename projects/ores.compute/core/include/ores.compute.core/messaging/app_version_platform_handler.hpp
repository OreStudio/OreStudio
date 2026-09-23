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
#ifndef ORES_COMPUTE_CORE_MESSAGING_APP_VERSION_PLATFORM_HANDLER_HPP
#define ORES_COMPUTE_CORE_MESSAGING_APP_VERSION_PLATFORM_HANDLER_HPP

#include "ores.compute.api/messaging/app_version_platform_protocol.hpp"
#include "ores.compute.core/service/app_version_platform_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::compute::messaging {

namespace {
inline auto& app_version_platform_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.compute.messaging.app_version_platform_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for app version platforms operations.
 *
 * The adapter decides nothing: it proves the request, checks the permission a
 * write needs, decodes the canonical request, calls the service and replies
 * with the response the service filled. The outcome a caller reads -- missing,
 * conflicting, denied -- is the service's answer, so the two cannot disagree
 * about what happened.
 */
class app_version_platform_handler {
public:
    app_version_platform_handler(ores::nats::service::client& nats,
                                 ores::database::context ctx,
                                 std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves compute.v1.app_version_platforms.list.
     */
    void list_app_version_platforms(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<list_app_version_platforms_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::app_version_platform_service svc(req_ctx);
        try {
            auto response = svc.list_app_version_platforms(*req);
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_app_version_platforms_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves compute.v1.app_version_platforms.get.
     */
    void get_app_version_platform(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<get_app_version_platform_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::app_version_platform_service svc(req_ctx);
        try {
            auto response = svc.get_app_version_platform(*req);
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_app_version_platform_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves compute.v1.app_version_platforms.get_many.
     */
    void get_many_app_version_platforms(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<get_many_app_version_platforms_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::app_version_platform_service svc(req_ctx);
        try {
            auto response = svc.get_many_app_version_platforms(*req);
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_many_app_version_platforms_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves compute.v1.app_version_platforms.put.
     */
    void put_app_version_platform(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "compute::app_version_platforms:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_app_version_platform_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::app_version_platform_service svc(req_ctx);
        try {
            auto response = svc.put_app_version_platform(*req);
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_app_version_platform_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves compute.v1.app_version_platforms.put_many.
     */
    void put_many_app_version_platforms(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "compute::app_version_platforms:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_many_app_version_platforms_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::app_version_platform_service svc(req_ctx);
        try {
            auto response = svc.put_many_app_version_platforms(*req);
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_many_app_version_platforms_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves compute.v1.app_version_platforms.delete.
     */
    void delete_app_version_platform(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "compute::app_version_platforms:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_app_version_platform_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::app_version_platform_service svc(req_ctx);
        try {
            auto response = svc.delete_app_version_platform(*req);
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_app_version_platform_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves compute.v1.app_version_platforms.delete_many.
     */
    void delete_many_app_version_platforms(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "compute::app_version_platforms:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_many_app_version_platforms_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::app_version_platform_service svc(req_ctx);
        try {
            auto response = svc.delete_many_app_version_platforms(*req);
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_many_app_version_platforms_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves compute.v1.app_version_platforms.list_by_app_version_id.
     */
    void list_by_app_version_id_app_version_platforms(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        auto req = decode<list_by_app_version_id_app_version_platforms_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::app_version_platform_service svc(req_ctx);
        try {
            auto response = svc.list_by_app_version_id_app_version_platforms(*req);
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_by_app_version_id_app_version_platforms_response failure;
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

} // namespace ores::compute::messaging

#endif
