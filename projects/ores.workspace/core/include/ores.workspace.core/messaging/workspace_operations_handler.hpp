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
#ifndef ORES_WORKSPACE_MESSAGING_WORKSPACE_OPERATIONS_HANDLER_HPP
#define ORES_WORKSPACE_MESSAGING_WORKSPACE_OPERATIONS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.workspace.api/messaging/workspace_operations_protocol.hpp"
#include "ores.workspace.core/repository/workspace_repository.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <rfl/json.hpp>
#include <vector>

namespace ores::workspace::messaging {

namespace {
inline auto& workspace_operations_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.workspace.messaging.workspace_operations_handler");
    return instance;
}
} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief Serves the workspace operations that are not entity verbs.
 *
 * The entity's own verbs come from the generated handler. These three are
 * hand-written because the resolution chain calls a set-returning SQL function
 * and the trade scope writes a table no entity model describes.
 */
class workspace_operations_handler {
public:
    workspace_operations_handler(ores::nats::service::client& nats,
                                 ores::database::context ctx,
                                 std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void resolve(ores::nats::message msg) {
        BOOST_LOG_SEV(workspace_operations_handler_lg(), debug) << "Handling " << msg.subject;

        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;

        if (!has_permission(ctx, "workspace::workspaces:read")) {
            BOOST_LOG_SEV(workspace_operations_handler_lg(), warn)
                << "Permission denied: workspace::workspaces:read";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<resolve_workspace_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(workspace_operations_handler_lg(), warn)
                << "Failed to decode resolve_workspace_request";
            reply(nats_, msg, resolve_workspace_response{});
            return;
        }

        try {
            repository::workspace_repository repo;
            reply(nats_,
                  msg,
                  resolve_workspace_response{
                      .resolution_order = repo.resolution_order(ctx, req->workspace_id)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(workspace_operations_handler_lg(), error)
                << "Error resolving workspace " << req->workspace_id << ": " << e.what();
            reply(nats_, msg, resolve_workspace_response{});
        }

        BOOST_LOG_SEV(workspace_operations_handler_lg(), debug) << "Completed " << msg.subject;
    }

    void set_trade_scope(ores::nats::message msg) {
        BOOST_LOG_SEV(workspace_operations_handler_lg(), debug) << "Handling " << msg.subject;

        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;

        if (!has_permission(ctx, "workspace::workspaces:write")) {
            BOOST_LOG_SEV(workspace_operations_handler_lg(), warn)
                << "Permission denied: workspace::workspaces:write";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<set_trade_scope_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(workspace_operations_handler_lg(), warn)
                << "Failed to decode set_trade_scope_request";
            reply(nats_,
                  msg,
                  set_trade_scope_response{.success = false, .message = "Failed to decode request"});
            return;
        }

        try {
            boost::uuids::string_generator gen;
            std::vector<boost::uuids::uuid> trade_ids;
            trade_ids.reserve(req->trade_ids.size());
            for (const auto& id : req->trade_ids)
                trade_ids.push_back(gen(id));

            repository::workspace_repository repo;
            repo.set_trade_scope(ctx, req->workspace_id, trade_ids);
            reply(nats_, msg, set_trade_scope_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(workspace_operations_handler_lg(), error)
                << "Error setting trade scope for " << req->workspace_id << ": " << e.what();
            reply(nats_, msg, set_trade_scope_response{.success = false, .message = e.what()});
        }

        BOOST_LOG_SEV(workspace_operations_handler_lg(), debug) << "Completed " << msg.subject;
    }

    void clear_trade_scope(ores::nats::message msg) {
        BOOST_LOG_SEV(workspace_operations_handler_lg(), debug) << "Handling " << msg.subject;

        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;

        if (!has_permission(ctx, "workspace::workspaces:write")) {
            BOOST_LOG_SEV(workspace_operations_handler_lg(), warn)
                << "Permission denied: workspace::workspaces:write";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<clear_trade_scope_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(workspace_operations_handler_lg(), warn)
                << "Failed to decode clear_trade_scope_request";
            reply(nats_,
                  msg,
                  clear_trade_scope_response{.success = false,
                                             .message = "Failed to decode request"});
            return;
        }

        try {
            repository::workspace_repository repo;
            repo.clear_trade_scope(ctx, req->workspace_id);
            reply(nats_, msg, clear_trade_scope_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(workspace_operations_handler_lg(), error)
                << "Error clearing trade scope for " << req->workspace_id << ": " << e.what();
            reply(nats_, msg, clear_trade_scope_response{.success = false, .message = e.what()});
        }

        BOOST_LOG_SEV(workspace_operations_handler_lg(), debug) << "Completed " << msg.subject;
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::workspace::messaging

#endif
