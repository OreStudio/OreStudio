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
#include "ores.refdata.core/messaging/bundle_import_operations_handler.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/messaging/bundle_import_operations_protocol.hpp"
#include "ores.refdata.core/service/bundle_import_operations_service.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <exception>
#include <optional>
#include <utility>

namespace ores::refdata::messaging {

using namespace ores::logging;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;

namespace {
inline auto& bundle_import_operations_handler_lg() {
    static auto instance = make_logger("ores.refdata.messaging.bundle_import_operations_handler");
    return instance;
}
}

bundle_import_operations_handler::bundle_import_operations_handler(
    ores::nats::service::client& nats,
    const ores::database::context& ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier)
    : nats_(nats)
    , ctx_(ctx)
    , verifier_(verifier) {}

void bundle_import_operations_handler::begin(ores::nats::message msg) {
    ORES_LOG_SEV(bundle_import_operations_handler_lg(), debug) << "Handling " << msg.subject;
    auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
    if (!req_ctx_expected) {
        error_reply(nats_, msg, req_ctx_expected.error());
        return;
    }
    const auto& req_ctx = *req_ctx_expected;
    if (!has_permission(req_ctx, "refdata::bundle_imports:write")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    if (auto req = decode<begin_bundle_import_request>(msg)) {
        try {
            service::bundle_import_operations_service svc(req_ctx);
            const auto response = svc.begin(*req);
            ORES_LOG_SEV(bundle_import_operations_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            ORES_LOG_SEV(bundle_import_operations_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, begin_bundle_import_response{.success = false, .message = e.what()});
        }
    } else {
        ORES_LOG_SEV(bundle_import_operations_handler_lg(), warn)
            << "Failed to decode: " << msg.subject;
        error_reply(nats_, msg, ores::service::error_code::bad_request);
    }
}

void bundle_import_operations_handler::stage(ores::nats::message msg) {
    ORES_LOG_SEV(bundle_import_operations_handler_lg(), debug) << "Handling " << msg.subject;
    auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
    if (!req_ctx_expected) {
        error_reply(nats_, msg, req_ctx_expected.error());
        return;
    }
    const auto& req_ctx = *req_ctx_expected;
    if (!has_permission(req_ctx, "refdata::bundle_imports:write")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    if (auto req = decode<stage_bundle_family_request>(msg)) {
        try {
            service::bundle_import_operations_service svc(req_ctx);
            const auto response = svc.stage(*req);
            ORES_LOG_SEV(bundle_import_operations_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            ORES_LOG_SEV(bundle_import_operations_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, stage_bundle_family_response{.success = false, .message = e.what()});
        }
    } else {
        ORES_LOG_SEV(bundle_import_operations_handler_lg(), warn)
            << "Failed to decode: " << msg.subject;
        error_reply(nats_, msg, ores::service::error_code::bad_request);
    }
}

void bundle_import_operations_handler::commit(ores::nats::message msg) {
    ORES_LOG_SEV(bundle_import_operations_handler_lg(), debug) << "Handling " << msg.subject;
    auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
    if (!req_ctx_expected) {
        error_reply(nats_, msg, req_ctx_expected.error());
        return;
    }
    const auto& req_ctx = *req_ctx_expected;
    if (!has_permission(req_ctx, "refdata::bundle_imports:write")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    if (auto req = decode<commit_bundle_import_request>(msg)) {
        try {
            service::bundle_import_operations_service svc(req_ctx);
            const auto response = svc.commit(*req);
            ORES_LOG_SEV(bundle_import_operations_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            ORES_LOG_SEV(bundle_import_operations_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, commit_bundle_import_response{.success = false, .message = e.what()});
        }
    } else {
        ORES_LOG_SEV(bundle_import_operations_handler_lg(), warn)
            << "Failed to decode: " << msg.subject;
        error_reply(nats_, msg, ores::service::error_code::bad_request);
    }
}

} // namespace ores::refdata::messaging
