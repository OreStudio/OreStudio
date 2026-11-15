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
#ifndef ORES_SYNTHETIC_CORE_MESSAGING_VARIANCE_CALIBRATION_INPUT_POINT_HANDLER_HPP
#define ORES_SYNTHETIC_CORE_MESSAGING_VARIANCE_CALIBRATION_INPUT_POINT_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/messaging/variance_calibration_input_point_protocol.hpp"
#include "ores.synthetic.core/service/variance_calibration_input_point_service.hpp"
#include <optional>

namespace ores::synthetic::messaging {

namespace {
inline auto& variance_calibration_input_point_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.synthetic.messaging.variance_calibration_input_point_handler");
    return instance;
}
}

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for variance calibration input point operations.
 */
class variance_calibration_input_point_handler {
public:
    variance_calibration_input_point_handler(
        ores::nats::service::client& nats,
        const ores::database::context& ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(verifier) {}

    /**
     * @brief Serves synthetic.v1.variance_calibration_input_points.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_variance_calibration_input_points(ores::nats::message msg) {
        BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            variance_calibration_input_point_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "synthetic::variance_calibration_input_points:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_variance_calibration_input_points_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::variance_calibration_input_point_service svc(req_ctx);
        try {
            auto response = svc.list_variance_calibration_input_points(*req);
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_variance_calibration_input_points_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves synthetic.v1.variance_calibration_input_points.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_variance_calibration_input_point(ores::nats::message msg) {
        BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            variance_calibration_input_point_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "synthetic::variance_calibration_input_points:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_variance_calibration_input_point_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::variance_calibration_input_point_service svc(req_ctx);
        try {
            auto response = svc.get_variance_calibration_input_point(*req);
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_variance_calibration_input_point_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves synthetic.v1.variance_calibration_input_points.get_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_many_variance_calibration_input_points(ores::nats::message msg) {
        BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            variance_calibration_input_point_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "synthetic::variance_calibration_input_points:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_many_variance_calibration_input_points_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::variance_calibration_input_point_service svc(req_ctx);
        try {
            auto response = svc.get_many_variance_calibration_input_points(*req);
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_many_variance_calibration_input_points_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves synthetic.v1.variance_calibration_input_points.put.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_variance_calibration_input_point(ores::nats::message msg) {
        BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            variance_calibration_input_point_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "synthetic::variance_calibration_input_points:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_variance_calibration_input_point_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::variance_calibration_input_point_service svc(req_ctx);
        try {
            auto response = svc.put_variance_calibration_input_point(*req);
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_variance_calibration_input_point_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves synthetic.v1.variance_calibration_input_points.put_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_many_variance_calibration_input_points(ores::nats::message msg) {
        BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            variance_calibration_input_point_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "synthetic::variance_calibration_input_points:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_many_variance_calibration_input_points_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::variance_calibration_input_point_service svc(req_ctx);
        try {
            auto response = svc.put_many_variance_calibration_input_points(*req);
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_many_variance_calibration_input_points_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves synthetic.v1.variance_calibration_input_points.delete.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_variance_calibration_input_point(ores::nats::message msg) {
        BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            variance_calibration_input_point_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "synthetic::variance_calibration_input_points:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_variance_calibration_input_point_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::variance_calibration_input_point_service svc(req_ctx);
        try {
            auto response = svc.delete_variance_calibration_input_point(*req);
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_variance_calibration_input_point_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves synthetic.v1.variance_calibration_input_points.delete_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_many_variance_calibration_input_points(ores::nats::message msg) {
        BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            variance_calibration_input_point_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "synthetic::variance_calibration_input_points:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_many_variance_calibration_input_points_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::variance_calibration_input_point_service svc(req_ctx);
        try {
            auto response = svc.delete_many_variance_calibration_input_points(*req);
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_many_variance_calibration_input_points_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves synthetic.v1.variance_calibration_input_points_versions.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_variance_calibration_input_point_versions(ores::nats::message msg) {
        BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            variance_calibration_input_point_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "synthetic::variance_calibration_input_points:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_variance_calibration_input_point_versions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::variance_calibration_input_point_service svc(req_ctx);
        try {
            auto response = svc.list_variance_calibration_input_point_versions(*req);
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_variance_calibration_input_point_versions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves synthetic.v1.variance_calibration_input_points_versions.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_variance_calibration_input_point_version(ores::nats::message msg) {
        BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            variance_calibration_input_point_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "synthetic::variance_calibration_input_points:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_variance_calibration_input_point_version_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::variance_calibration_input_point_service svc(req_ctx);
        try {
            auto response = svc.get_variance_calibration_input_point_version(*req);
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(variance_calibration_input_point_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_variance_calibration_input_point_version_response failure;
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
