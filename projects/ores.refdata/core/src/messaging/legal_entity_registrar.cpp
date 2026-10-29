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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_nats_registrar.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.refdata.core/messaging/legal_entity_registrar.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.refdata.api/messaging/legal_entity_protocol.hpp"
#include "ores.refdata.core/messaging/legal_entity_handler.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <memory>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::refdata::messaging {

namespace {
static constexpr std::string_view legal_entity_queue_group = "ores.refdata.service";
}

std::vector<ores::nats::service::subscription>
register_legal_entity_handlers(ores::nats::service::client& nats,
                               ores::database::context ctx,
                               std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    auto h = std::make_shared<legal_entity_handler>(nats, std::move(ctx), std::move(verifier));
    subs.push_back(nats.queue_subscribe(
        list_legal_entities_request::nats_subject,
        legal_entity_queue_group,
        [h](ores::nats::message msg) { h->list_legal_entities(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_legal_entity_request::nats_subject,
        legal_entity_queue_group,
        [h](ores::nats::message msg) { h->get_legal_entity(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_many_legal_entities_request::nats_subject,
        legal_entity_queue_group,
        [h](ores::nats::message msg) { h->get_many_legal_entities(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        put_legal_entity_request::nats_subject,
        legal_entity_queue_group,
        [h](ores::nats::message msg) { h->put_legal_entity(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        put_many_legal_entities_request::nats_subject,
        legal_entity_queue_group,
        [h](ores::nats::message msg) { h->put_many_legal_entities(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_legal_entity_request::nats_subject,
        legal_entity_queue_group,
        [h](ores::nats::message msg) { h->delete_legal_entity(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_many_legal_entities_request::nats_subject,
        legal_entity_queue_group,
        [h](ores::nats::message msg) { h->delete_many_legal_entities(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        list_legal_entity_versions_request::nats_subject,
        legal_entity_queue_group,
        [h](ores::nats::message msg) { h->list_legal_entity_versions(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_legal_entity_version_request::nats_subject,
        legal_entity_queue_group,
        [h](ores::nats::message msg) { h->get_legal_entity_version(std::move(msg)); }));
    return subs;
}

}
