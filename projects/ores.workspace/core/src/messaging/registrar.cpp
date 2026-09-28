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
#include "ores.history.core/messaging/registrar.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.workspace.api/messaging/workspace_operations_protocol.hpp"
#include "ores.workspace.core/messaging/workspace_history_provider_registrar.hpp"
#include "ores.workspace.core/messaging/workspace_operations_handler.hpp"
#include "ores.workspace.core/messaging/workspace_registrar.hpp"
#include <memory>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::workspace::messaging {

namespace {

constexpr std::string_view queue_group = "ores.workspace.service";

// The registry must outlive the history.v1.get subscription, and
// register_handlers is only ever called once per service process.
ores::history::service::dispatch_registry& history_registry() {
    static ores::history::service::dispatch_registry instance;
    return instance;
}

} // namespace

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier) {

    std::vector<ores::nats::service::subscription> subs;

    // The workspace entity stack is generated: the CRUD, version and change
    // verbs come from the entity registrar.
    for (auto& sub : register_workspace_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));

    // Resolution and trade scope are operations rather than entity verbs, so
    // their handlers are hand-written beside the operation model that declares
    // their subjects.
    {
        auto h = std::make_shared<workspace_operations_handler>(nats, ctx, verifier);
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
    }

    // Workspace history comes from the generic history provider.
    {
        auto& hist_registry = history_registry();
        register_workspace_history_provider(hist_registry);
        subs.push_back(ores::history::messaging::register_history_handlers(
            nats, hist_registry, "workspace", queue_group, ctx, verifier));
    }

    return subs;
}

} // namespace ores::workspace::messaging
