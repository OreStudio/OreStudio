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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_MARKET_DATA_MAPPING_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_MARKET_DATA_MAPPING_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/market_data_mapping_protocol.hpp"
#include "ores.marketdata.core/service/market_data_mapping_service.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::marketdata::messaging {

namespace {
inline auto& market_data_mapping_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.market_data_mapping_handler");
    return instance;
}
}

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for market data mapping operations.
 */
class market_data_mapping_handler {
public:
    market_data_mapping_handler(ores::nats::service::client& nats,
                                ores::database::context ctx,
                                std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves marketdata.v1.market_data_mappings.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_market_data_mappings(ores::nats::message msg) {
        BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            market_data_mapping_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "marketdata::market_data_mappings:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_market_data_mappings_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_data_mapping_service svc(req_ctx);
        try {
            auto response = svc.list_market_data_mappings(*req);
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_market_data_mappings_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_data_mappings.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_market_data_mapping(ores::nats::message msg) {
        BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            market_data_mapping_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "marketdata::market_data_mappings:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_market_data_mapping_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_data_mapping_service svc(req_ctx);
        try {
            auto response = svc.get_market_data_mapping(*req);
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_market_data_mapping_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_data_mappings.get_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_many_market_data_mappings(ores::nats::message msg) {
        BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            market_data_mapping_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "marketdata::market_data_mappings:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_many_market_data_mappings_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_data_mapping_service svc(req_ctx);
        try {
            auto response = svc.get_many_market_data_mappings(*req);
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_many_market_data_mappings_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_data_mappings.put.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_market_data_mapping(ores::nats::message msg) {
        BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            market_data_mapping_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "marketdata::market_data_mappings:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_market_data_mapping_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_data_mapping_service svc(req_ctx);
        try {
            auto response = svc.put_market_data_mapping(*req);
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_market_data_mapping_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_data_mappings.put_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_many_market_data_mappings(ores::nats::message msg) {
        BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            market_data_mapping_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "marketdata::market_data_mappings:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_many_market_data_mappings_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_data_mapping_service svc(req_ctx);
        try {
            auto response = svc.put_many_market_data_mappings(*req);
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_many_market_data_mappings_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_data_mappings.delete.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_market_data_mapping(ores::nats::message msg) {
        BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            market_data_mapping_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "marketdata::market_data_mappings:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_market_data_mapping_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_data_mapping_service svc(req_ctx);
        try {
            auto response = svc.delete_market_data_mapping(*req);
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_market_data_mapping_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_data_mappings.delete_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_many_market_data_mappings(ores::nats::message msg) {
        BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            market_data_mapping_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "marketdata::market_data_mappings:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_many_market_data_mappings_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_data_mapping_service svc(req_ctx);
        try {
            auto response = svc.delete_many_market_data_mappings(*req);
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_many_market_data_mappings_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_data_mappings_versions.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_market_data_mapping_versions(ores::nats::message msg) {
        BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            market_data_mapping_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "marketdata::market_data_mappings:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_market_data_mapping_versions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_data_mapping_service svc(req_ctx);
        try {
            auto response = svc.list_market_data_mapping_versions(*req);
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_market_data_mapping_versions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_data_mappings_versions.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_market_data_mapping_version(ores::nats::message msg) {
        BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        [[maybe_unused]] const auto correlation_id = ores::service::messaging::log_handler_entry(
            market_data_mapping_handler_lg(), msg, req_ctx.tenant_id().to_string());
        if (!has_permission(req_ctx, "marketdata::market_data_mappings:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_market_data_mapping_version_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_data_mapping_service svc(req_ctx);
        try {
            auto response = svc.get_market_data_mapping_version(*req);
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_data_mapping_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_market_data_mapping_version_response failure;
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
