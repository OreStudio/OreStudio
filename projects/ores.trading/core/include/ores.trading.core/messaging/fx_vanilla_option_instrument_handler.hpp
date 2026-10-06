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
#ifndef ORES_TRADING_CORE_MESSAGING_FX_VANILLA_OPTION_INSTRUMENT_HANDLER_HPP
#define ORES_TRADING_CORE_MESSAGING_FX_VANILLA_OPTION_INSTRUMENT_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.trading.api/messaging/fx_vanilla_option_instrument_protocol.hpp"
#include "ores.trading.core/service/fx_vanilla_option_instrument_service.hpp"
#include <optional>

namespace ores::trading::messaging {

namespace {
inline auto& fx_vanilla_option_instrument_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.trading.messaging.fx_vanilla_option_instrument_handler");
    return instance;
}
}

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for FX vanilla option instrument operations.
 */
class fx_vanilla_option_instrument_handler {
public:
    fx_vanilla_option_instrument_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves trading.v1.fx_vanilla_option_instruments.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_fx_vanilla_option_instruments(ores::nats::message msg) {
        BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::fx_vanilla_option_instruments:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_fx_vanilla_option_instruments_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::fx_vanilla_option_instrument_service svc(req_ctx);
        try {
            auto response = svc.list_fx_vanilla_option_instruments(*req);
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_fx_vanilla_option_instruments_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.fx_vanilla_option_instruments.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_fx_vanilla_option_instrument(ores::nats::message msg) {
        BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::fx_vanilla_option_instruments:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_fx_vanilla_option_instrument_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::fx_vanilla_option_instrument_service svc(req_ctx);
        try {
            auto response = svc.get_fx_vanilla_option_instrument(*req);
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_fx_vanilla_option_instrument_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.fx_vanilla_option_instruments.get_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_many_fx_vanilla_option_instruments(ores::nats::message msg) {
        BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::fx_vanilla_option_instruments:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_many_fx_vanilla_option_instruments_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::fx_vanilla_option_instrument_service svc(req_ctx);
        try {
            auto response = svc.get_many_fx_vanilla_option_instruments(*req);
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_many_fx_vanilla_option_instruments_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.fx_vanilla_option_instruments.put.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_fx_vanilla_option_instrument(ores::nats::message msg) {
        BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::fx_vanilla_option_instruments:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_fx_vanilla_option_instrument_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::fx_vanilla_option_instrument_service svc(req_ctx);
        try {
            auto response = svc.put_fx_vanilla_option_instrument(*req);
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_fx_vanilla_option_instrument_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.fx_vanilla_option_instruments.put_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_many_fx_vanilla_option_instruments(ores::nats::message msg) {
        BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::fx_vanilla_option_instruments:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_many_fx_vanilla_option_instruments_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::fx_vanilla_option_instrument_service svc(req_ctx);
        try {
            auto response = svc.put_many_fx_vanilla_option_instruments(*req);
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_many_fx_vanilla_option_instruments_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.fx_vanilla_option_instruments.delete.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_fx_vanilla_option_instrument(ores::nats::message msg) {
        BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::fx_vanilla_option_instruments:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_fx_vanilla_option_instrument_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::fx_vanilla_option_instrument_service svc(req_ctx);
        try {
            auto response = svc.delete_fx_vanilla_option_instrument(*req);
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_fx_vanilla_option_instrument_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.fx_vanilla_option_instruments.delete_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_many_fx_vanilla_option_instruments(ores::nats::message msg) {
        BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::fx_vanilla_option_instruments:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_many_fx_vanilla_option_instruments_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::fx_vanilla_option_instrument_service svc(req_ctx);
        try {
            auto response = svc.delete_many_fx_vanilla_option_instruments(*req);
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_many_fx_vanilla_option_instruments_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.fx_vanilla_option_instruments_versions.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_fx_vanilla_option_instrument_versions(ores::nats::message msg) {
        BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::fx_vanilla_option_instruments:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_fx_vanilla_option_instrument_versions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::fx_vanilla_option_instrument_service svc(req_ctx);
        try {
            auto response = svc.list_fx_vanilla_option_instrument_versions(*req);
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_fx_vanilla_option_instrument_versions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.fx_vanilla_option_instruments_versions.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_fx_vanilla_option_instrument_version(ores::nats::message msg) {
        BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::fx_vanilla_option_instruments:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_fx_vanilla_option_instrument_version_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::fx_vanilla_option_instrument_service svc(req_ctx);
        try {
            auto response = svc.get_fx_vanilla_option_instrument_version(*req);
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(fx_vanilla_option_instrument_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_fx_vanilla_option_instrument_version_response failure;
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
