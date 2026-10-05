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
#include "ores.reporting.core/messaging/run_document_handler.hpp"
#include "ores.reporting.api/messaging/run_document_protocol.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/service/run_document_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>
#include <string>
#include <utility>

namespace ores::reporting::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;

namespace {

/**
 * @brief The context to act in for a definition, and the definition's id.
 *
 * Throws when the session cannot see the definition, or acts for a party that
 * does not own it.
 */
std::pair<ores::database::context, boost::uuids::uuid>
owner_scope(const ores::database::context& ctx, const std::string& report_definition_id) {
    boost::uuids::uuid id;
    try {
        id = boost::uuids::string_generator()(report_definition_id);
    } catch (const std::exception&) {
        throw std::invalid_argument("Not a report definition id: " + report_definition_id);
    }
    const auto found =
        repository::report_definition_repository().read_latest(ctx, boost::uuids::to_string(id));
    if (found.empty())
        throw std::invalid_argument("No report definition " + report_definition_id +
                                    " is visible to the session.");
    const auto owner = found.front().party_id;
    if (const auto party = ctx.party_id()) {
        if (*party != owner)
            throw std::invalid_argument(
                "The report definition belongs to another party; act for that party to change "
                "its run.");
        return {ctx, id};
    }
    return {ctx.with_party(ctx.tenant_id(), owner, {owner}, ctx.actor()), id};
}

}

run_document_handler::run_document_handler(
    ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , verifier_(std::move(verifier)) {}

std::optional<ores::database::context>
run_document_handler::authorise(const ores::nats::message& msg, std::string_view permission) {
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

void run_document_handler::save_run_document(ores::nats::message msg) {
    const auto ctx = authorise(msg, "reporting::report_run_setups:write");
    if (!ctx)
        return;
    const auto req = decode<save_run_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        const auto [scope, definition] = owner_scope(*ctx, req->report_definition_id);
        service::run_document_service runs(scope);
        runs.save(definition, req->document);
        reply(nats_, msg, save_run_document_response{.success = true});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, save_run_document_response{.message = e.what()});
    }
}

void run_document_handler::bind_configuration(ores::nats::message msg) {
    const auto ctx = authorise(msg, "reporting::configurations:write");
    if (!ctx)
        return;
    const auto req = decode<bind_configuration_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        const auto [scope, definition] = owner_scope(*ctx, req->report_definition_id);
        service::run_document_service runs(scope);
        const auto c = runs.bind(definition, req->configuration_type_code, req->name);
        reply(nats_,
              msg,
              bind_configuration_response{.success = true,
                                          .configuration_id = boost::uuids::to_string(c.id)});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, bind_configuration_response{.message = e.what()});
    }
}

void run_document_handler::get_run_document(ores::nats::message msg) {
    const auto ctx = authorise(msg, "reporting::report_run_setups:read");
    if (!ctx)
        return;
    const auto req = decode<get_run_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        const auto [scope, definition] = owner_scope(*ctx, req->report_definition_id);
        service::run_document_service runs(scope);
        const auto document = runs.get(definition);
        if (!document) {
            reply(nats_,
                  msg,
                  get_run_document_response{.message =
                                                "The report definition holds no run document."});
            return;
        }
        reply(nats_,
              msg,
              get_run_document_response{.success = true,
                                        .document = *document,
                                        .bindings = runs.bindings(definition),
                                        .party_id = boost::uuids::to_string(*scope.party_id())});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, get_run_document_response{.message = e.what()});
    }
}

void run_document_handler::delete_run_document(ores::nats::message msg) {
    const auto ctx = authorise(msg, "reporting::report_run_setups:delete");
    if (!ctx)
        return;
    const auto req = decode<delete_run_document_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        const auto [scope, definition] = owner_scope(*ctx, req->report_definition_id);
        service::run_document_service runs(scope);
        runs.remove(definition);
        reply(nats_, msg, delete_run_document_response{.success = true});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << msg.subject << " refused: " << e.what();
        reply(nats_, msg, delete_run_document_response{.message = e.what()});
    }
}

}
