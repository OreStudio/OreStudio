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
#ifndef ORES_TRADING_CORE_MESSAGING_BOND_ISSUE_LEG_RATE_HANDLER_HPP
#define ORES_TRADING_CORE_MESSAGING_BOND_ISSUE_LEG_RATE_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.trading.api/messaging/bond_issue_leg_rate_protocol.hpp"
#include "ores.trading.core/service/bond_issue_leg_rate_service.hpp"
#include <optional>

namespace ores::trading::messaging {

namespace {
inline auto& bond_issue_leg_rate_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.trading.messaging.bond_issue_leg_rate_handler");
    return instance;
}
}

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for bond issue leg rate operations.
 */
class bond_issue_leg_rate_handler {
public:
    bond_issue_leg_rate_handler(ores::nats::service::client& nats,
                                ores::database::context ctx,
                                std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves trading.v1.bond_issue_leg_rates.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_bond_issue_leg_rates(ores::nats::message msg) {
        BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::bond_issue_leg_rates:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_bond_issue_leg_rates_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::bond_issue_leg_rate_service svc(req_ctx);
        try {
            auto response = svc.list_bond_issue_leg_rates(*req);
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_bond_issue_leg_rates_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.bond_issue_leg_rates.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_bond_issue_leg_rate(ores::nats::message msg) {
        BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::bond_issue_leg_rates:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_bond_issue_leg_rate_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::bond_issue_leg_rate_service svc(req_ctx);
        try {
            auto response = svc.get_bond_issue_leg_rate(*req);
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_bond_issue_leg_rate_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.bond_issue_leg_rates.get_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_many_bond_issue_leg_rates(ores::nats::message msg) {
        BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::bond_issue_leg_rates:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_many_bond_issue_leg_rates_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::bond_issue_leg_rate_service svc(req_ctx);
        try {
            auto response = svc.get_many_bond_issue_leg_rates(*req);
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_many_bond_issue_leg_rates_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.bond_issue_leg_rates.put.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_bond_issue_leg_rate(ores::nats::message msg) {
        BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::bond_issue_leg_rates:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_bond_issue_leg_rate_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::bond_issue_leg_rate_service svc(req_ctx);
        try {
            auto response = svc.put_bond_issue_leg_rate(*req);
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_bond_issue_leg_rate_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.bond_issue_leg_rates.put_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_many_bond_issue_leg_rates(ores::nats::message msg) {
        BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::bond_issue_leg_rates:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_many_bond_issue_leg_rates_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::bond_issue_leg_rate_service svc(req_ctx);
        try {
            auto response = svc.put_many_bond_issue_leg_rates(*req);
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_many_bond_issue_leg_rates_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.bond_issue_leg_rates.delete.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_bond_issue_leg_rate(ores::nats::message msg) {
        BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::bond_issue_leg_rates:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_bond_issue_leg_rate_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::bond_issue_leg_rate_service svc(req_ctx);
        try {
            auto response = svc.delete_bond_issue_leg_rate(*req);
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_bond_issue_leg_rate_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.bond_issue_leg_rates.delete_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_many_bond_issue_leg_rates(ores::nats::message msg) {
        BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::bond_issue_leg_rates:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_many_bond_issue_leg_rates_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::bond_issue_leg_rate_service svc(req_ctx);
        try {
            auto response = svc.delete_many_bond_issue_leg_rates(*req);
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_many_bond_issue_leg_rates_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.bond_issue_leg_rates_versions.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_bond_issue_leg_rate_versions(ores::nats::message msg) {
        BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::bond_issue_leg_rates:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_bond_issue_leg_rate_versions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::bond_issue_leg_rate_service svc(req_ctx);
        try {
            auto response = svc.list_bond_issue_leg_rate_versions(*req);
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_bond_issue_leg_rate_versions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves trading.v1.bond_issue_leg_rates_versions.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_bond_issue_leg_rate_version(ores::nats::message msg) {
        BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "trading::bond_issue_leg_rates:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_bond_issue_leg_rate_version_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::bond_issue_leg_rate_service svc(req_ctx);
        try {
            auto response = svc.get_bond_issue_leg_rate_version(*req);
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(bond_issue_leg_rate_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_bond_issue_leg_rate_version_response failure;
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
