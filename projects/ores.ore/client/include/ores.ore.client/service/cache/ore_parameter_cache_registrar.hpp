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
#ifndef ORES_ORE_CLIENT_SERVICE_CACHE_ORE_PARAMETER_CACHE_REGISTRAR_HPP
#define ORES_ORE_CLIENT_SERVICE_CACHE_ORE_PARAMETER_CACHE_REGISTRAR_HPP

#include "ores.eventing.api/domain/entity_event.hpp"
#include "ores.eventing.api/domain/entity_event_traits.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.ore.api/eventing/ore_parameter_event.hpp"
#include "ores.ore.client/service/cache/ore_parameter_cache.hpp"
#include <memory>
#include <string>
#include <thread>
#include <vector>

namespace ores::ore::service::cache {

namespace {
inline auto& ore_parameter_cache_registrar_lg() {
    static auto instance =
        ores::logging::make_logger("ores.ore.service.cache.ore_parameter_cache_registrar");
    return instance;
}
}

/**
 * @brief Warms ore_parameter_cache for every given tenant, then
 * subscribes to the ore_parameter created, updated and deleted
 * event subjects so each affected tenant is reloaded on any mutation.
 * Call once at service startup and keep every returned subscription
 * alive for the service's lifetime.
 */
inline std::vector<ores::nats::service::subscription>
warm_and_subscribe_ore_parameter_cache(ores::nats::service::client& nats,
                                       std::shared_ptr<ore_parameter_cache> cache,
                                       const std::vector<std::string>& tenant_ids) {
    using namespace ores::logging;
    ORES_LOG_SEV(ore_parameter_cache_registrar_lg(), debug)
        << "Warming ore_parameter cache for " << tenant_ids.size() << " tenant(s)";
    for (const auto& tenant_id : tenant_ids)
        // A warm-up failure is logged; nothing here can react to it.
        (void)cache->load(tenant_id);

    using ores::eventing::domain::event_subject;
    using ores::ore::messaging::ore_parameter_event;

    const auto on_change = [cache](ores::nats::message msg) {
        // The payload is the event alone; the tenant that owns the row is in
        // the envelope, so an event without it names no partition to reload.
        const auto tenant = msg.headers.find(std::string(ores::nats::headers::x_tenant_id));
        if (tenant != msg.headers.end() && !tenant->second.empty()) {
            // Offload to a detached thread: load() calls request_sync,
            // which would block the NATS callback thread if called inline.
            std::thread([cache, tid = tenant->second]() { (void)cache->load(tid); }).detach();
        }
    };

    std::vector<ores::nats::service::subscription> subs;
    subs.reserve(3);
    subs.push_back(nats.subscribe(event_subject<ore_parameter_event>("created"), on_change));
    subs.push_back(nats.subscribe(event_subject<ore_parameter_event>("updated"), on_change));
    subs.push_back(nats.subscribe(event_subject<ore_parameter_event>("deleted"), on_change));
    return subs;
}

}

#endif
