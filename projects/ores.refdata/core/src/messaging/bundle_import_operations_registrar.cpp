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
#include "ores.refdata.core/messaging/bundle_import_operations_registrar.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.refdata.api/messaging/bundle_import_operations_protocol.hpp"
#include "ores.refdata.core/messaging/bundle_import_operations_handler.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <memory>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::refdata::messaging {

namespace {
static constexpr std::string_view bundle_import_operations_queue_group = "ores.refdata.service";
}

std::vector<ores::nats::service::subscription> register_bundle_import_operations_handlers(
    ores::nats::service::client& nats,
    const ores::database::context& ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    auto h = std::make_shared<bundle_import_operations_handler>(nats, ctx, std::move(verifier));
    subs.push_back(
        nats.queue_subscribe(begin_bundle_import_request::nats_subject,
                             bundle_import_operations_queue_group,
                             [h](ores::nats::message msg) { h->begin(std::move(msg)); }));
    subs.push_back(
        nats.queue_subscribe(stage_bundle_family_request::nats_subject,
                             bundle_import_operations_queue_group,
                             [h](ores::nats::message msg) { h->stage(std::move(msg)); }));
    subs.push_back(
        nats.queue_subscribe(commit_bundle_import_request::nats_subject,
                             bundle_import_operations_queue_group,
                             [h](ores::nats::message msg) { h->commit(std::move(msg)); }));
    return subs;
}

} // namespace ores::refdata::messaging
