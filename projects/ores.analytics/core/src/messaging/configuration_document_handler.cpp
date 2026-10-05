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
#include "ores.analytics.core/messaging/configuration_document_handler.hpp"
#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.analytics.core/service/pricing_engines_document_service.hpp"
#include "ores.analytics.core/service/todays_market_document_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>
#include <string>

namespace ores::analytics::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

namespace {

boost::uuids::uuid parse_id(const std::string& text) {
    try {
        return boost::uuids::string_generator()(text);
    } catch (const std::exception&) {
        throw std::invalid_argument("Not an id: " + text);
    }
}

}

configuration_document_handler::configuration_document_handler(
    ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , verifier_(std::move(verifier)) {}

std::optional<ores::database::context>
configuration_document_handler::authorise(const ores::nats::message& msg,
                                          std::string_view permission) {
    auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
    if (!ctx) {
        error_reply(nats_, msg, ctx.error());
        return std::nullopt;
    }
    if (!has_permission(*ctx, permission)) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return std::nullopt;
    }
    return *ctx;
}

void configuration_document_handler::save_pricing_engines_document(ores::nats::message msg) {
    const auto ctx = authorise(msg, "analytics::pricing_model_configs:write");
    if (!ctx)
        return;
    const auto req = decode<save_pricing_engines_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        service::pricing_engines_document_service(*ctx).save(req->document);
        reply(nats_,
              msg,
              save_pricing_engines_document_response{
                  .success = true, .id = boost::uuids::to_string(req->document.config.id)});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, save_pricing_engines_document_response{.message = e.what()});
    }
}

void configuration_document_handler::get_pricing_engines_document(ores::nats::message msg) {
    const auto ctx = authorise(msg, "analytics::pricing_model_configs:read");
    if (!ctx)
        return;
    const auto req = decode<get_pricing_engines_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        service::pricing_engines_document_service svc(for_requested_party(*ctx, req->party_id));
        const auto id = svc.find_by_configuration(parse_id(req->configuration_id));
        if (!id) {
            reply(nats_,
                  msg,
                  get_pricing_engines_document_response{
                      .message = "No pricing engines document fills configuration " +
                                 req->configuration_id + "."});
            return;
        }
        reply(nats_,
              msg,
              get_pricing_engines_document_response{.success = true, .document = svc.get(*id)});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, get_pricing_engines_document_response{.message = e.what()});
    }
}

void configuration_document_handler::delete_pricing_engines_document(ores::nats::message msg) {
    const auto ctx = authorise(msg, "analytics::pricing_model_configs:delete");
    if (!ctx)
        return;
    const auto req = decode<delete_pricing_engines_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        service::pricing_engines_document_service svc(*ctx);
        if (const auto id = svc.find_by_configuration(parse_id(req->configuration_id)))
            svc.remove(*id);
        reply(nats_, msg, delete_pricing_engines_document_response{.success = true});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, delete_pricing_engines_document_response{.message = e.what()});
    }
}

void configuration_document_handler::save_todays_market_document(ores::nats::message msg) {
    const auto ctx = authorise(msg, "analytics::todays_market_configs:write");
    if (!ctx)
        return;
    const auto req = decode<save_todays_market_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        service::todays_market_document_service(*ctx).save(req->document);
        reply(nats_,
              msg,
              save_todays_market_document_response{
                  .success = true, .id = boost::uuids::to_string(req->document.config.id)});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, save_todays_market_document_response{.message = e.what()});
    }
}

void configuration_document_handler::get_todays_market_document(ores::nats::message msg) {
    const auto ctx = authorise(msg, "analytics::todays_market_configs:read");
    if (!ctx)
        return;
    const auto req = decode<get_todays_market_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        service::todays_market_document_service svc(for_requested_party(*ctx, req->party_id));
        const auto id = svc.find_by_configuration(parse_id(req->configuration_id));
        if (!id) {
            reply(nats_,
                  msg,
                  get_todays_market_document_response{
                      .message = "No today's market document fills configuration " +
                                 req->configuration_id + "."});
            return;
        }
        reply(nats_,
              msg,
              get_todays_market_document_response{.success = true, .document = svc.get(*id)});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, get_todays_market_document_response{.message = e.what()});
    }
}

void configuration_document_handler::delete_todays_market_document(ores::nats::message msg) {
    const auto ctx = authorise(msg, "analytics::todays_market_configs:delete");
    if (!ctx)
        return;
    const auto req = decode<delete_todays_market_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        service::todays_market_document_service svc(*ctx);
        if (const auto id = svc.find_by_configuration(parse_id(req->configuration_id)))
            svc.remove(*id);
        reply(nats_, msg, delete_todays_market_document_response{.success = true});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, delete_todays_market_document_response{.message = e.what()});
    }
}

}
