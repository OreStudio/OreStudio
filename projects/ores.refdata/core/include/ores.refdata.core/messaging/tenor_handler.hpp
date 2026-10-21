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
#ifndef ORES_REFDATA_CORE_MESSAGING_TENOR_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_TENOR_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/tenor_protocol.hpp"
#include "ores.refdata.core/service/tenor_resolution_service.hpp"
#include "ores.refdata.core/service/tenor_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::refdata::messaging {

namespace {
inline auto& tenor_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.refdata.messaging.tenor_handler");
    return instance;
}
}

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for tenor operations.
 */
class tenor_handler {
public:
    tenor_handler(ores::nats::service::client& nats,
                  ores::database::context ctx,
                  std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves refdata.v1.tenors.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_tenors(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            tenor_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "refdata::tenors:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_tenors_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_service svc(req_ctx);
        try {
            auto response = svc.list_tenors(*req);
            BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_handler_lg(), error) << msg.subject << " failed: " << e.what();
            list_tenors_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.tenors.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_tenor(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            tenor_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "refdata::tenors:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_tenor_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_service svc(req_ctx);
        try {
            auto response = svc.get_tenor(*req);
            BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_tenor_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.tenors.get_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_many_tenors(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            tenor_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "refdata::tenors:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_many_tenors_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_service svc(req_ctx);
        try {
            auto response = svc.get_many_tenors(*req);
            BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_many_tenors_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.tenors.put.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_tenor(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            tenor_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "refdata::tenors:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_tenor_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_service svc(req_ctx);
        try {
            auto response = svc.put_tenor(*req);
            BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_handler_lg(), error) << msg.subject << " failed: " << e.what();
            put_tenor_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.tenors.put_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_many_tenors(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            tenor_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "refdata::tenors:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_many_tenors_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_service svc(req_ctx);
        try {
            auto response = svc.put_many_tenors(*req);
            BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_handler_lg(), error) << msg.subject << " failed: " << e.what();
            put_many_tenors_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.tenors.delete.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_tenor(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            tenor_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "refdata::tenors:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_tenor_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_service svc(req_ctx);
        try {
            auto response = svc.delete_tenor(*req);
            BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_handler_lg(), error) << msg.subject << " failed: " << e.what();
            delete_tenor_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.tenors.delete_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_many_tenors(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            tenor_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "refdata::tenors:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_many_tenors_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_service svc(req_ctx);
        try {
            auto response = svc.delete_many_tenors(*req);
            BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_handler_lg(), error) << msg.subject << " failed: " << e.what();
            delete_many_tenors_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.tenors_versions.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_tenor_versions(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            tenor_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "refdata::tenors:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_tenor_versions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_service svc(req_ctx);
        try {
            auto response = svc.list_tenor_versions(*req);
            BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_handler_lg(), error) << msg.subject << " failed: " << e.what();
            list_tenor_versions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.tenors_versions.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_tenor_version(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            tenor_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "refdata::tenors:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_tenor_version_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_service svc(req_ctx);
        try {
            auto response = svc.get_tenor_version(*req);
            BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_tenor_version_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.ops.resolve_tenor.
     *
     * A refusal is a normal typed reply carrying a non-ok =result=, not a
     * thrown exception: a caller reads which step refused, the dates it had
     * counted to, and the row at fault, out of the payload. Only the
     * transport failing -- no session, no permission, an undecodable
     * request -- is an error reply.
     */
    void resolve_tenor(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            tenor_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "refdata::tenors:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<resolve_tenor_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_resolution_service svc(req_ctx);
        try {
            auto response = svc.resolve(*req);
            BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(tenor_handler_lg(), error) << msg.subject << " failed: " << e.what();
            resolve_tenor_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.ops.preview_tenor_resolution.
     *
     * The same question as resolve_tenor, asked about rows that are not
     * stored yet, so a person editing a rule reads its answer before writing
     * it. Same response, same refusals, same adapter: it proves the request,
     * checks the permission, decodes, calls the service and replies.
     */
    void preview_tenor_resolution(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            tenor_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "refdata::tenors:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<preview_tenor_resolution_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_resolution_service svc(req_ctx);
        try {
            auto response = svc.preview(*req);
            BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenor_handler_lg(), error) << msg.subject << " failed: " << e.what();
            resolve_tenor_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves refdata.v1.ops.resolve_convention.
     *
     * The same question as resolve_tenor, asked once for every tenor the
     * convention holds, so a screen that lays out the term structure makes one
     * call. A tenor that refuses is a refusal in its own entry; only a
     * convention or calendar that cannot be read at all refuses the whole
     * reply.
     */
    void resolve_convention(ores::nats::message msg) {
        BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            tenor_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "refdata::tenors:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<resolve_convention_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenor_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::tenor_resolution_service svc(req_ctx);
        try {
            auto response = svc.resolve_convention(*req);
            BOOST_LOG_SEV(tenor_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenor_handler_lg(), error) << msg.subject << " failed: " << e.what();
            resolve_convention_response failure;
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
