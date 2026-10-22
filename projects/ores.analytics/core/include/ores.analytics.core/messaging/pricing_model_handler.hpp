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
#ifndef ORES_ANALYTICS_CORE_MESSAGING_PRICING_MODEL_HANDLER_HPP
#define ORES_ANALYTICS_CORE_MESSAGING_PRICING_MODEL_HANDLER_HPP

#include "ores.analytics.api/messaging/pricing_model_protocol.hpp"
#include "ores.analytics.core/service/pricing_model_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <optional>

namespace ores::analytics::messaging {

namespace {
inline auto& pricing_model_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.analytics.messaging.pricing_model_handler");
    return instance;
}
}

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for pricing model operations.
 *
 * Pricing Models are system-owned global entities; list and history
 * operations use the system tenant context.
 */
class pricing_model_handler {
public:
    pricing_model_handler(ores::nats::service::client& nats,
                          ores::database::context ctx,
                          std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves analytics.v1.pricing_models.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_pricing_models(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            pricing_model_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "analytics::pricing_models:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_pricing_models_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(pricing_model_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        const auto sys_ctx =
            req_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), req_ctx.actor());
        service::pricing_model_service svc(sys_ctx);
        try {
            auto response = svc.list_pricing_models(*req);
            BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(pricing_model_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_pricing_models_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves analytics.v1.pricing_models.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_pricing_model(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            pricing_model_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "analytics::pricing_models:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_pricing_model_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(pricing_model_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        const auto sys_ctx =
            req_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), req_ctx.actor());
        service::pricing_model_service svc(sys_ctx);
        try {
            auto response = svc.get_pricing_model(*req);
            BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(pricing_model_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_pricing_model_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves analytics.v1.pricing_models.get_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_many_pricing_models(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            pricing_model_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "analytics::pricing_models:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_many_pricing_models_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(pricing_model_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        const auto sys_ctx =
            req_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), req_ctx.actor());
        service::pricing_model_service svc(sys_ctx);
        try {
            auto response = svc.get_many_pricing_models(*req);
            BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(pricing_model_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_many_pricing_models_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves analytics.v1.pricing_models.put.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_pricing_model(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            pricing_model_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "analytics::pricing_models:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_pricing_model_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(pricing_model_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        const auto sys_ctx =
            req_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), req_ctx.actor());
        service::pricing_model_service svc(sys_ctx);
        try {
            auto response = svc.put_pricing_model(*req);
            BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(pricing_model_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_pricing_model_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves analytics.v1.pricing_models.put_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_many_pricing_models(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            pricing_model_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "analytics::pricing_models:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_many_pricing_models_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(pricing_model_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        const auto sys_ctx =
            req_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), req_ctx.actor());
        service::pricing_model_service svc(sys_ctx);
        try {
            auto response = svc.put_many_pricing_models(*req);
            BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(pricing_model_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_many_pricing_models_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves analytics.v1.pricing_models.delete.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_pricing_model(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            pricing_model_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "analytics::pricing_models:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_pricing_model_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(pricing_model_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        const auto sys_ctx =
            req_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), req_ctx.actor());
        service::pricing_model_service svc(sys_ctx);
        try {
            auto response = svc.delete_pricing_model(*req);
            BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(pricing_model_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_pricing_model_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves analytics.v1.pricing_models.delete_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_many_pricing_models(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            pricing_model_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "analytics::pricing_models:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_many_pricing_models_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(pricing_model_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        const auto sys_ctx =
            req_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), req_ctx.actor());
        service::pricing_model_service svc(sys_ctx);
        try {
            auto response = svc.delete_many_pricing_models(*req);
            BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(pricing_model_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_many_pricing_models_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves analytics.v1.pricing_models_versions.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_pricing_model_versions(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            pricing_model_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "analytics::pricing_models:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_pricing_model_versions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(pricing_model_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        const auto sys_ctx =
            req_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), req_ctx.actor());
        service::pricing_model_service svc(sys_ctx);
        try {
            auto response = svc.list_pricing_model_versions(*req);
            BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(pricing_model_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_pricing_model_versions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves analytics.v1.pricing_models_versions.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_pricing_model_version(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            pricing_model_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "analytics::pricing_models:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_pricing_model_version_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(pricing_model_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        const auto sys_ctx =
            req_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), req_ctx.actor());
        service::pricing_model_service svc(sys_ctx);
        try {
            auto response = svc.get_pricing_model_version(*req);
            BOOST_LOG_SEV(pricing_model_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(pricing_model_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_pricing_model_version_response failure;
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

}

#endif
