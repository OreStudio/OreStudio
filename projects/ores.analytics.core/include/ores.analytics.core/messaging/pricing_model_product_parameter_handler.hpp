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
#ifndef ORES_ANALYTICS_MESSAGING_PRICING_MODEL_PRODUCT_PARAMETER_HANDLER_HPP
#define ORES_ANALYTICS_MESSAGING_PRICING_MODEL_PRODUCT_PARAMETER_HANDLER_HPP

#include <optional>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.analytics.api/messaging/pricing_model_product_parameter_protocol.hpp"
#include "ores.analytics.core/service/pricing_model_product_parameter_service.hpp"

namespace ores::analytics::messaging {

namespace {
inline auto& pricing_model_product_parameter_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.analytics.messaging.pricing_model_product_parameter_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for pricing model product parameter operations.
 */
class pricing_model_product_parameter_handler {
public:
    pricing_model_product_parameter_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::pricing_model_product_parameter_service svc(req_ctx);
        get_pricing_model_product_parameters_response resp;
        if (auto req = decode<get_pricing_model_product_parameters_request>(msg)) {
            try {
                resp.parameters = svc.list_parameters(req->config_id);
                resp.total_available_count =
                    static_cast<int>(resp.parameters.size());
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
        }
        BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "analytics::pricing_model_product_parameters:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::pricing_model_product_parameter_service svc(req_ctx);
        if (auto req = decode<save_pricing_model_product_parameter_request>(msg)) {
            try {
                svc.save_parameter(req->data);
                BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg,
                    save_pricing_model_product_parameter_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, save_pricing_model_product_parameter_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::pricing_model_product_parameter_service svc(req_ctx);
        if (auto req = decode<get_pricing_model_product_parameter_history_request>(msg)) {
            try {
                auto hist = svc.get_parameter_history(boost::uuids::to_string(req->id));
                BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, get_pricing_model_product_parameter_history_response{
                    .parameters = std::move(hist), .success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, get_pricing_model_product_parameter_history_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "analytics::pricing_model_product_parameters:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::pricing_model_product_parameter_service svc(req_ctx);
        if (auto req = decode<delete_pricing_model_product_parameter_request>(msg)) {
            try {
                for (const auto& id : req->ids)
                    svc.remove_parameter(boost::uuids::to_string(id));
                BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg,
                    delete_pricing_model_product_parameter_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, delete_pricing_model_product_parameter_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void list_for_config(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::pricing_model_product_parameter_service svc(req_ctx);
        get_pricing_model_product_parameters_for_config_response resp;
        try {
            if (auto req = decode<get_pricing_model_product_parameters_for_config_request>(msg)) {
                resp.parameters = svc.list_parameters(req->config_id);
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void list_for_product(ores::nats::message msg) {
        BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::pricing_model_product_parameter_service svc(req_ctx);
        get_pricing_model_product_parameters_for_product_response resp;
        try {
            if (auto req = decode<get_pricing_model_product_parameters_for_product_request>(msg)) {
                resp.parameters = svc.list_parameters_for_product(req->product_id);
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        BOOST_LOG_SEV(pricing_model_product_parameter_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::analytics::messaging

#endif
