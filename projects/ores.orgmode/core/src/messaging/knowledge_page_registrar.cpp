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
#include "ores.orgmode.core/messaging/knowledge_page_registrar.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.orgmode.api/messaging/knowledge_page_protocol.hpp"
#include "ores.orgmode.core/messaging/knowledge_page_handler.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <memory>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::orgmode::messaging {

namespace {
static constexpr std::string_view knowledge_page_queue_group = "ores.orgmode.service";
}

std::vector<ores::nats::service::subscription>
register_knowledge_page_handlers(ores::nats::service::client& nats,
                                 const ores::database::context& ctx,
                                 std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    auto h = std::make_shared<knowledge_page_handler>(nats, ctx, std::move(verifier));
    subs.push_back(nats.queue_subscribe(
        list_knowledge_pages_request::nats_subject,
        knowledge_page_queue_group,
        [h](ores::nats::message msg) { h->list_knowledge_pages(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_knowledge_page_request::nats_subject,
        knowledge_page_queue_group,
        [h](ores::nats::message msg) { h->get_knowledge_page(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_many_knowledge_pages_request::nats_subject,
        knowledge_page_queue_group,
        [h](ores::nats::message msg) { h->get_many_knowledge_pages(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        list_knowledge_page_versions_request::nats_subject,
        knowledge_page_queue_group,
        [h](ores::nats::message msg) { h->list_knowledge_page_versions(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_knowledge_page_version_request::nats_subject,
        knowledge_page_queue_group,
        [h](ores::nats::message msg) { h->get_knowledge_page_version(std::move(msg)); }));
    return subs;
}

}
