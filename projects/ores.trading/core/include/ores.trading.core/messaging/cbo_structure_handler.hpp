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
#ifndef ORES_TRADING_CORE_MESSAGING_CBO_STRUCTURE_HANDLER_HPP
#define ORES_TRADING_CORE_MESSAGING_CBO_STRUCTURE_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.trading.api/messaging/cbo_structure_protocol.hpp"
#include "ores.trading.core/service/cbo_structure_service.hpp"
#include <optional>

namespace ores::trading::messaging {

namespace {
inline auto& cbo_structure_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.trading.messaging.cbo_structure_handler");
    return instance;
}
}

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for cbo structure operations.
 */
class cbo_structure_handler {
public:
    cbo_structure_handler(ores::nats::service::client& nats,
                          const ores::database::context& ctx,
                          std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(verifier) {}

    /**
     * @brief Serves trading.v1.cbo_structures.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_cbo_structures(ores::nats::message msg) {
        ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            cbo_structure_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "trading::cbo_structures:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_cbo_structures_request>(msg);
        if (!req) {
            ORES_LOG_SEV(cbo_structure_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::cbo_structure_service svc(req_ctx);
        try {
            auto response = svc.list_cbo_structures(*req);
            ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            ORES_LOG_SEV(cbo_structure_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_cbo_structures_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.cbo_structures.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_cbo_structure(ores::nats::message msg) {
        ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            cbo_structure_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "trading::cbo_structures:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_cbo_structure_request>(msg);
        if (!req) {
            ORES_LOG_SEV(cbo_structure_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::cbo_structure_service svc(req_ctx);
        try {
            auto response = svc.get_cbo_structure(*req);
            ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            ORES_LOG_SEV(cbo_structure_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_cbo_structure_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.cbo_structures.get_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_many_cbo_structures(ores::nats::message msg) {
        ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            cbo_structure_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "trading::cbo_structures:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_many_cbo_structures_request>(msg);
        if (!req) {
            ORES_LOG_SEV(cbo_structure_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::cbo_structure_service svc(req_ctx);
        try {
            auto response = svc.get_many_cbo_structures(*req);
            ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            ORES_LOG_SEV(cbo_structure_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_many_cbo_structures_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.cbo_structures.put.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_cbo_structure(ores::nats::message msg) {
        ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            cbo_structure_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "trading::cbo_structures:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_cbo_structure_request>(msg);
        if (!req) {
            ORES_LOG_SEV(cbo_structure_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::cbo_structure_service svc(req_ctx);
        try {
            auto response = svc.put_cbo_structure(*req);
            ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            ORES_LOG_SEV(cbo_structure_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_cbo_structure_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.cbo_structures.put_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_many_cbo_structures(ores::nats::message msg) {
        ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            cbo_structure_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "trading::cbo_structures:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_many_cbo_structures_request>(msg);
        if (!req) {
            ORES_LOG_SEV(cbo_structure_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::cbo_structure_service svc(req_ctx);
        try {
            auto response = svc.put_many_cbo_structures(*req);
            ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            ORES_LOG_SEV(cbo_structure_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_many_cbo_structures_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.cbo_structures.delete.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_cbo_structure(ores::nats::message msg) {
        ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            cbo_structure_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "trading::cbo_structures:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_cbo_structure_request>(msg);
        if (!req) {
            ORES_LOG_SEV(cbo_structure_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::cbo_structure_service svc(req_ctx);
        try {
            auto response = svc.delete_cbo_structure(*req);
            ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            ORES_LOG_SEV(cbo_structure_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_cbo_structure_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.cbo_structures.delete_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_many_cbo_structures(ores::nats::message msg) {
        ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            cbo_structure_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "trading::cbo_structures:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_many_cbo_structures_request>(msg);
        if (!req) {
            ORES_LOG_SEV(cbo_structure_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::cbo_structure_service svc(req_ctx);
        try {
            auto response = svc.delete_many_cbo_structures(*req);
            ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            ORES_LOG_SEV(cbo_structure_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_many_cbo_structures_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.cbo_structures.list_by_trade_id.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_by_trade_id_cbo_structures(ores::nats::message msg) {
        ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            cbo_structure_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "trading::cbo_structures:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_by_trade_id_cbo_structures_request>(msg);
        if (!req) {
            ORES_LOG_SEV(cbo_structure_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::cbo_structure_service svc(req_ctx);
        try {
            auto response = svc.list_by_trade_id_cbo_structures(*req);
            ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            ORES_LOG_SEV(cbo_structure_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_by_trade_id_cbo_structures_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.cbo_structures_versions.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_cbo_structure_versions(ores::nats::message msg) {
        ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            cbo_structure_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "trading::cbo_structures:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_cbo_structure_versions_request>(msg);
        if (!req) {
            ORES_LOG_SEV(cbo_structure_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::cbo_structure_service svc(req_ctx);
        try {
            auto response = svc.list_cbo_structure_versions(*req);
            ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            ORES_LOG_SEV(cbo_structure_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_cbo_structure_versions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.cbo_structures_versions.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_cbo_structure_version(ores::nats::message msg) {
        ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            cbo_structure_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "trading::cbo_structures:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_cbo_structure_version_request>(msg);
        if (!req) {
            ORES_LOG_SEV(cbo_structure_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::cbo_structure_service svc(req_ctx);
        try {
            auto response = svc.get_cbo_structure_version(*req);
            ORES_LOG_SEV(cbo_structure_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            ORES_LOG_SEV(cbo_structure_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_cbo_structure_version_response failure;
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
