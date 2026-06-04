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
#ifndef ORES_WORKSPACE_CORE_MESSAGING_WORKSPACE_HANDLER_HPP
#define ORES_WORKSPACE_CORE_MESSAGING_WORKSPACE_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.workspace.api/messaging/workspace_protocol.hpp"
#include "ores.workspace.core/service/workspace_service.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <thread>
#include <vector>

namespace ores::workspace::messaging {

namespace {
inline auto& workspace_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.workspace.core.messaging.workspace_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class workspace_handler {
public:
    workspace_handler(ores::nats::service::client& nats,
                      ores::database::context ctx,
                      std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(workspace_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "workspace::workspaces:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::workspace_service svc(ctx);
        list_workspaces_response resp;
        auto req = decode<list_workspaces_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(workspace_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(nats_, msg, resp);
            return;
        }
        try {
            resp.workspaces = svc.list_workspaces();
            BOOST_LOG_SEV(workspace_handler_lg(), debug) << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(workspace_handler_lg(), error) << msg.subject << " failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    void create(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(workspace_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "workspace::workspaces:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::workspace_service svc(ctx);
        auto req = decode<create_workspace_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(workspace_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(
                nats_,
                msg,
                create_workspace_response{.success = false, .message = "Failed to decode request"});
            return;
        }
        try {
            const auto id = svc.create_workspace(req->data);
            BOOST_LOG_SEV(workspace_handler_lg(), debug)
                << "Completed " << msg.subject << " id=" << id;
            reply(nats_, msg, create_workspace_response{.success = true, .id = id});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(workspace_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, create_workspace_response{.success = false, .message = e.what()});
        }
    }

    void archive(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(workspace_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        auto req = decode<archive_workspace_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(workspace_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(nats_,
                  msg,
                  archive_workspace_response{.success = false,
                                             .message = "Failed to decode request"});
            return;
        }

        // Three-tier permission check per design:
        //   live workspace    → workspace::live_workspace:archive  (SuperAdmin)
        //   caller's own ws   → workspace::workspaces:archive      (Trading+)
        //   another user's ws → workspace::workspaces:archive_any  (TenantAdmin+)
        const bool is_live = (req->id == ores::utility::uuid::live_workspace_uuid_str);

        service::workspace_service svc(ctx);

        if (is_live) {
            if (!has_permission(ctx, "workspace::live_workspace:archive")) {
                error_reply(nats_, msg, ores::service::error_code::forbidden);
                return;
            }
        } else {
            const auto ws = svc.get_workspace(req->id);
            const bool is_owner =
                ws.has_value() && ctx.party_id().has_value() && (ws->owner_id == *ctx.party_id());
            const auto required =
                is_owner ? "workspace::workspaces:archive" : "workspace::workspaces:archive_any";
            if (!has_permission(ctx, required)) {
                error_reply(nats_, msg, ores::service::error_code::forbidden);
                return;
            }
        }

        try {
            svc.archive_workspace(
                req->id, req->modified_by, req->change_reason_code, req->change_commentary);
            BOOST_LOG_SEV(workspace_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, archive_workspace_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(workspace_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, archive_workspace_response{.success = false, .message = e.what()});
        }
    }

    void resolve(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(workspace_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::workspace_service svc(ctx);
        resolve_workspace_response resp;
        auto req = decode<resolve_workspace_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(workspace_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(nats_, msg, resp);
            return;
        }
        try {
            resp.resolution_order = svc.resolve(req->workspace_id);
            BOOST_LOG_SEV(workspace_handler_lg(), debug) << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(workspace_handler_lg(), error) << msg.subject << " failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    void remove(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(workspace_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        auto req = decode<remove_workspace_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(workspace_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(
                nats_,
                msg,
                remove_workspace_response{.success = false, .message = "Failed to decode request"});
            return;
        }

        // Three-tier permission: same ownership logic as archive, but :delete permissions.
        const bool is_live = (req->id == ores::utility::uuid::live_workspace_uuid_str);

        if (is_live) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        service::workspace_service svc(ctx);
        const auto ws = svc.get_workspace(req->id);
        const bool is_owner =
            ws.has_value() && ctx.party_id().has_value() && (ws->owner_id == *ctx.party_id());
        const auto required =
            is_owner ? "workspace::workspaces:delete" : "workspace::workspaces:delete_any";
        if (!has_permission(ctx, required)) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        try {
            svc.remove_workspace(req->id);
            BOOST_LOG_SEV(workspace_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, remove_workspace_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(workspace_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, remove_workspace_response{.success = false, .message = e.what()});
        }
    }

    void set_trade_scope(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(workspace_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "workspace::workspaces:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::workspace_service svc(ctx);
        auto req = decode<set_trade_scope_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(workspace_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(
                nats_,
                msg,
                set_trade_scope_response{.success = false, .message = "Failed to decode request"});
            return;
        }
        try {
            boost::uuids::string_generator gen;
            std::vector<boost::uuids::uuid> trade_uuids;
            trade_uuids.reserve(req->trade_ids.size());
            for (const auto& s : req->trade_ids)
                trade_uuids.push_back(gen(s));
            svc.set_trade_scope(req->workspace_id, trade_uuids);
            BOOST_LOG_SEV(workspace_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, set_trade_scope_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(workspace_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, set_trade_scope_response{.success = false, .message = e.what()});
        }
    }

    void history(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(workspace_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "workspace::workspaces:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_workspace_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(workspace_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(nats_, msg, get_workspace_history_response{});
            return;
        }
        std::thread([this, msg = std::move(msg), ctx, id = req->id]() mutable {
            service::workspace_service svc(ctx);
            get_workspace_history_response resp;
            try {
                resp.workspaces = svc.get_workspace_history(id);
                resp.success = true;
                BOOST_LOG_SEV(workspace_handler_lg(), debug) << "Completed " << msg.subject;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(workspace_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.message = e.what();
            }
            reply(nats_, msg, resp);
        }).detach();
    }

    void clear_trade_scope(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(workspace_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "workspace::workspaces:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::workspace_service svc(ctx);
        auto req = decode<clear_trade_scope_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(workspace_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(nats_,
                  msg,
                  clear_trade_scope_response{.success = false,
                                             .message = "Failed to decode request"});
            return;
        }
        try {
            svc.clear_trade_scope(req->workspace_id);
            BOOST_LOG_SEV(workspace_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, clear_trade_scope_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(workspace_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, clear_trade_scope_response{.success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::workspace::messaging
#endif
