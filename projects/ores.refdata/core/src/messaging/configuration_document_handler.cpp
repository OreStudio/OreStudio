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
#include "ores.refdata.core/messaging/configuration_document_handler.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.refdata.core/service/conventions_document_service.hpp"
#include "ores.refdata.core/service/curve_configuration_document_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/authorise.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <exception>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>

namespace ores::refdata::messaging {

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


void configuration_document_handler::save_curve_configuration_document(ores::nats::message msg) {
    const auto ctx = authorise(nats_, ctx_, msg, verifier_, "refdata::curve_configurations:write");
    if (!ctx)
        return;
    const auto req = decode<save_curve_configuration_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        require_party(*ctx);
        service::curve_configuration_document_service(*ctx).save(req->document);
        reply(nats_,
              msg,
              save_curve_configuration_document_response{
                  .success = true, .id = boost::uuids::to_string(req->document.config.id)});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, save_curve_configuration_document_response{.message = e.what()});
    }
}

void configuration_document_handler::get_curve_configuration_document(ores::nats::message msg) {
    const auto ctx = authorise(nats_, ctx_, msg, verifier_, "refdata::curve_configurations:read");
    if (!ctx)
        return;
    const auto req = decode<get_curve_configuration_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        service::curve_configuration_document_service svc(for_requested_party(*ctx, req->party_id));
        const auto id = svc.find_by_configuration(parse_id(req->configuration_id));
        if (!id) {
            reply(nats_,
                  msg,
                  get_curve_configuration_document_response{
                      .message = "No curve configuration document fills configuration " +
                                 req->configuration_id + "."});
            return;
        }
        reply(nats_,
              msg,
              get_curve_configuration_document_response{.success = true, .document = svc.get(*id)});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, get_curve_configuration_document_response{.message = e.what()});
    }
}

void configuration_document_handler::delete_curve_configuration_document(ores::nats::message msg) {
    const auto ctx = authorise(nats_, ctx_, msg, verifier_, "refdata::curve_configurations:delete");
    if (!ctx)
        return;
    const auto req = decode<delete_curve_configuration_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        require_party(*ctx);
        service::curve_configuration_document_service svc(*ctx);
        if (const auto id = svc.find_by_configuration(parse_id(req->configuration_id)))
            svc.remove(*id);
        reply(nats_, msg, delete_curve_configuration_document_response{.success = true});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, delete_curve_configuration_document_response{.message = e.what()});
    }
}

void configuration_document_handler::save_conventions_document(ores::nats::message msg) {
    const auto ctx = authorise(nats_, ctx_, msg, verifier_, "refdata::conventions:write");
    if (!ctx)
        return;
    const auto req = decode<save_conventions_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        require_party(*ctx);
        const auto saved = service::conventions_document_service(*ctx).save(req->document);
        reply(nats_,
              msg,
              save_conventions_document_response{
                  .success = true, .world_kept = saved.world_kept, .fx_skipped = saved.fx_skipped});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, save_conventions_document_response{.message = e.what()});
    }
}

void configuration_document_handler::get_conventions_document(ores::nats::message msg) {
    const auto ctx = authorise(nats_, ctx_, msg, verifier_, "refdata::conventions:read");
    if (!ctx)
        return;
    const auto req = decode<get_conventions_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        service::conventions_document_service svc(for_requested_party(*ctx, req->party_id));
        reply(
            nats_, msg, get_conventions_document_response{.success = true, .document = svc.get()});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, get_conventions_document_response{.message = e.what()});
    }
}

}
