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
#include "ores.scheduler.core/messaging/store_bounds_handler.hpp"
#include "ores.logging/logger.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <exception>

namespace ores::scheduler::messaging {

namespace {

auto& store_bounds_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.scheduler.messaging.store_bounds_handler");
    return instance;
}

constexpr std::string_view read_permission = "scheduler::job_instances:read";

}

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::reply;
using namespace ores::logging;

store_bounds_handler::store_bounds_handler(
    ores::nats::service::client& nats,
    const ores::database::context& ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier)
    : nats_(nats)
    , ctx_(std::make_shared<ores::database::context>(ctx))
    , verifier_(std::move(verifier)) {}

void store_bounds_handler::list(ores::nats::message msg) {
    ORES_LOG_SEV(store_bounds_handler_lg(), debug) << "Handling " << msg.subject;

    auto ctx_expected = ores::service::service::make_request_context(*ctx_, msg, verifier_);
    if (!ctx_expected) {
        error_reply(nats_, msg, ctx_expected.error());
        return;
    }
    const auto& ctx = *ctx_expected;
    if (!ores::service::messaging::has_permission(ctx, read_permission)) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }

    if (!decode<list_store_bounds_request>(msg)) {
        ORES_LOG_SEV(store_bounds_handler_lg(), warn) << "Failed to decode the request";
        reply(nats_,
              msg,
              list_store_bounds_response{.success = false, .message = "Failed to decode request"});
        return;
    }

    list_store_bounds_response resp;
    try {
        repository::store_bounds_repository repo(ctx);
        resp.stores = repo.read_bounds();
    } catch (const std::exception& e) {
        ORES_LOG_SEV(store_bounds_handler_lg(), error) << "Reading the bounds failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }
    reply(nats_, msg, resp);
}

void store_bounds_handler::samples(ores::nats::message msg) {
    ORES_LOG_SEV(store_bounds_handler_lg(), debug) << "Handling " << msg.subject;

    auto ctx_expected = ores::service::service::make_request_context(*ctx_, msg, verifier_);
    if (!ctx_expected) {
        error_reply(nats_, msg, ctx_expected.error());
        return;
    }
    const auto& ctx = *ctx_expected;
    if (!ores::service::messaging::has_permission(ctx, read_permission)) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }

    const auto request = decode<list_store_samples_request>(msg);
    if (!request) {
        ORES_LOG_SEV(store_bounds_handler_lg(), warn) << "Failed to decode the request";
        reply(nats_,
              msg,
              list_store_samples_response{.success = false, .message = "Failed to decode request"});
        return;
    }

    list_store_samples_response resp;
    try {
        repository::store_bounds_repository repo(ctx);
        auto series = repo.read_samples(request->store_code, request->range);
        resp.samples = std::move(series.samples);
        resp.from_at = std::move(series.from_at);
        resp.to_at = std::move(series.to_at);
    } catch (const std::exception& e) {
        ORES_LOG_SEV(store_bounds_handler_lg(), error)
            << "Reading the measurements failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }
    reply(nats_, msg, resp);
}

void store_bounds_handler::tables(ores::nats::message msg) {
    ORES_LOG_SEV(store_bounds_handler_lg(), debug) << "Handling " << msg.subject;

    auto ctx_expected = ores::service::service::make_request_context(*ctx_, msg, verifier_);
    if (!ctx_expected) {
        error_reply(nats_, msg, ctx_expected.error());
        return;
    }
    const auto& ctx = *ctx_expected;
    if (!ores::service::messaging::has_permission(ctx, read_permission)) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }

    const auto request = decode<list_largest_relations_request>(msg);
    if (!request) {
        ORES_LOG_SEV(store_bounds_handler_lg(), warn) << "Failed to decode the request";
        reply(nats_,
              msg,
              list_largest_relations_response{.success = false,
                                              .message = "Failed to decode request"});
        return;
    }

    list_largest_relations_response resp;
    try {
        repository::store_bounds_repository repo(ctx);
        auto page = repo.read_largest_relations(request->limit);
        resp.rows = std::move(page.rows);
        resp.database_bytes = page.database_bytes;
        resp.relation_count = page.relation_count;
        resp.read_at = std::move(page.read_at);
    } catch (const std::exception& e) {
        ORES_LOG_SEV(store_bounds_handler_lg(), error)
            << "Reading the largest relations failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }
    reply(nats_, msg, resp);
}

void store_bounds_handler::capacity(ores::nats::message msg) {
    ORES_LOG_SEV(store_bounds_handler_lg(), debug) << "Handling " << msg.subject;

    auto ctx_expected = ores::service::service::make_request_context(*ctx_, msg, verifier_);
    if (!ctx_expected) {
        error_reply(nats_, msg, ctx_expected.error());
        return;
    }
    const auto& ctx = *ctx_expected;
    if (!ores::service::messaging::has_permission(ctx, read_permission)) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }

    const auto request = decode<list_capacity_samples_request>(msg);
    if (!request) {
        ORES_LOG_SEV(store_bounds_handler_lg(), warn) << "Failed to decode the request";
        reply(nats_,
              msg,
              list_capacity_samples_response{.success = false,
                                             .message = "Failed to decode request"});
        return;
    }

    list_capacity_samples_response resp;
    try {
        repository::store_bounds_repository repo(ctx);
        auto series = repo.read_capacity(request->range);
        resp.samples = std::move(series.samples);
        resp.from_at = std::move(series.from_at);
        resp.to_at = std::move(series.to_at);
    } catch (const std::exception& e) {
        ORES_LOG_SEV(store_bounds_handler_lg(), error)
            << "Reading the installation's capacity failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }
    reply(nats_, msg, resp);
}

}
