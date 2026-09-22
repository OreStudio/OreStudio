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
 * Template: cpp_nats_event_cache_registrar.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_CORE_SERVICE_CACHE_PARTY_CACHE_REGISTRAR_HPP
#define ORES_IAM_CORE_SERVICE_CACHE_PARTY_CACHE_REGISTRAR_HPP

#include "ores.eventing.api/domain/entity_event.hpp"
#include "ores.eventing.api/domain/entity_event_traits.hpp"
#include "ores.iam.core/service/cache/party_cache.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/eventing/party_event.hpp"
#include <memory>
#include <string>
#include <thread>
#include <vector>

namespace ores::iam::service::cache {

namespace {
inline auto& party_cache_registrar_lg() {
    static auto instance =
        ores::logging::make_logger("ores.iam.service.cache.party_cache_registrar");
    return instance;
}
} // namespace

/**
 * @brief Warms party_cache for every given tenant, then
 * subscribes to the party created, updated and deleted
 * event subjects so each affected tenant is reloaded on any mutation.
 * Call once at service startup and keep every returned subscription
 * alive for the service's lifetime.
 */
inline std::vector<ores::nats::service::subscription>
warm_and_subscribe_party_cache(ores::nats::service::client& nats,
                               std::shared_ptr<party_cache> cache,
                               const std::vector<std::string>& tenant_ids) {
    using namespace ores::logging;
    BOOST_LOG_SEV(party_cache_registrar_lg(), debug)
        << "Warming party cache for " << tenant_ids.size() << " tenant(s)";
    for (const auto& tenant_id : tenant_ids)
        (void)cache->load(tenant_id); // warm-up failure is logged; nothing here can react to it

    using ores::eventing::domain::entity_event_notification;
    using ores::eventing::domain::event_subject;
    using ores::refdata::messaging::party_event;

    const auto on_change = [cache](ores::nats::message msg) {
        auto evt = ores::nats::default_wire_codec().decode<entity_event_notification>(msg.data);
        if (evt && !evt->tenant_id.empty()) {
            // Offload to a detached thread: load() calls request_sync,
            // which would block the NATS callback thread if called inline.
            std::thread([cache, tid = evt->tenant_id]() { (void)cache->load(tid); }).detach();
        }
    };

    std::vector<ores::nats::service::subscription> subs;
    subs.reserve(3);
    subs.push_back(nats.subscribe(event_subject<party_event>("created"), on_change));
    subs.push_back(nats.subscribe(event_subject<party_event>("updated"), on_change));
    subs.push_back(nats.subscribe(event_subject<party_event>("deleted"), on_change));
    return subs;
}

} // namespace ores::iam::service::cache

#endif
