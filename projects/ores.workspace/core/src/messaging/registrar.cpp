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
#include "ores.workspace.core/messaging/registrar.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.workspace.api/messaging/workspace_protocol.hpp"
#include "ores.workspace.core/messaging/workspace_handler.hpp"
#include <memory>
#include <optional>

namespace ores::workspace::messaging {

namespace {
static constexpr std::string_view queue_group = "ores.workspace.service";
} // namespace

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier) {

    std::vector<ores::nats::service::subscription> subs;

    // ----------------------------------------------------------------
    // Workspaces
    // ----------------------------------------------------------------
    {
        auto h = std::make_shared<workspace_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            list_workspaces_request::nats_subject, queue_group, [h](ores::nats::message msg) {
                h->list(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            create_workspace_request::nats_subject, queue_group, [h](ores::nats::message msg) {
                h->create(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            archive_workspace_request::nats_subject, queue_group, [h](ores::nats::message msg) {
                h->archive(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            remove_workspace_request::nats_subject, queue_group, [h](ores::nats::message msg) {
                h->remove(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            resolve_workspace_request::nats_subject, queue_group, [h](ores::nats::message msg) {
                h->resolve(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            set_trade_scope_request::nats_subject, queue_group, [h](ores::nats::message msg) {
                h->set_trade_scope(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            clear_trade_scope_request::nats_subject, queue_group, [h](ores::nats::message msg) {
                h->clear_trade_scope(std::move(msg));
            }));
        subs.push_back(nats.queue_subscribe(
            get_workspace_history_request::nats_subject, queue_group, [h](ores::nats::message msg) {
                h->history(std::move(msg));
            }));
    }

    return subs;
}

} // namespace ores::workspace::messaging
