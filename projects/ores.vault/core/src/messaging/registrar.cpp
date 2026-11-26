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
#include "ores.vault.core/messaging/registrar.hpp"
#include "ores.vault.api/messaging/secret_operations_protocol.hpp"
#include "ores.vault.core/messaging/consumer_registrar.hpp"
#include "ores.vault.core/messaging/secret_operations_handler.hpp"
#include "ores.vault.core/messaging/secret_registrar.hpp"
#include "ores.vault.core/messaging/secret_use_registrar.hpp"
#include "ores.vault.core/security/credential_source.hpp"
#include <memory>
#include <string_view>
#include <utility>

namespace ores::vault::messaging {

namespace {

constexpr std::string_view custody_queue_group = "ores.vault.service";

}

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             const ores::database::context& ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    // Loading here, rather than in the application, is what makes a unit with
    // no usable key fail to start instead of serving a request it cannot answer.
    auto keys = std::make_shared<const security::key_material>(
        security::credential_source::load_from_credentials_directory());

    std::vector<ores::nats::service::subscription> subs;

    for (auto& sub : register_secret_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_consumer_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_secret_use_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));

    auto handler =
        std::make_shared<secret_operations_handler>(nats, ctx, std::move(verifier), keys);
    subs.push_back(nats.queue_subscribe(
        vault_public_key_request::nats_subject,
        custody_queue_group,
        [handler](ores::nats::message msg) { handler->public_key(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        register_consumer_request::nats_subject,
        custody_queue_group,
        [handler](ores::nats::message msg) { handler->register_consumer(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        write_secret_request::nats_subject,
        custody_queue_group,
        [handler](ores::nats::message msg) { handler->write(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        describe_secret_request::nats_subject,
        custody_queue_group,
        [handler](ores::nats::message msg) { handler->describe(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        resolve_secret_request::nats_subject,
        custody_queue_group,
        [handler](ores::nats::message msg) { handler->resolve(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        remove_secret_request::nats_subject,
        custody_queue_group,
        [handler](ores::nats::message msg) { handler->remove(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        rotate_secrets_request::nats_subject,
        custody_queue_group,
        [handler](ores::nats::message msg) { handler->rotate(std::move(msg)); }));

    return subs;
}

}
