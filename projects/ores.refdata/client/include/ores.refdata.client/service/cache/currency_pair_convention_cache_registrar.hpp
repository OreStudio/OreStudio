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
#ifndef ORES_REFDATA_CLIENT_SERVICE_CACHE_CURRENCY_PAIR_CONVENTION_CACHE_REGISTRAR_HPP
#define ORES_REFDATA_CLIENT_SERVICE_CACHE_CURRENCY_PAIR_CONVENTION_CACHE_REGISTRAR_HPP

#include "ores.eventing.api/domain/entity_change_event.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/eventing/currency_pair_convention_changed_event.hpp"
#include "ores.refdata.client/service/cache/currency_pair_convention_cache.hpp"
#include <memory>
#include <rfl/json.hpp>
#include <string>
#include <thread>
#include <vector>

namespace ores::refdata::service::cache {

namespace {
inline auto& currency_pair_convention_cache_registrar_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.refdata.service.cache.currency_pair_convention_cache_registrar");
    return instance;
}
} // namespace

/**
 * @brief Warms currency_pair_convention_cache for every given tenant, then
 * subscribes to the currency_pair_convention changed-event subject so each
 * affected tenant is reloaded on any mutation. Call once at service
 * startup and keep the returned subscription alive for the service's
 * lifetime.
 */
inline ores::nats::service::subscription warm_and_subscribe_currency_pair_convention_cache(
    ores::nats::service::client& nats,
    std::shared_ptr<currency_pair_convention_cache> cache,
    const std::vector<std::string>& tenant_ids) {
    using namespace ores::logging;
    BOOST_LOG_SEV(currency_pair_convention_cache_registrar_lg(), debug)
        << "Warming currency_pair_convention cache for " << tenant_ids.size() << " tenant(s)";
    for (const auto& tenant_id : tenant_ids)
        cache->load(tenant_id);

    using ores::eventing::domain::event_traits;
    using ores::refdata::eventing::currency_pair_convention_changed_event;
    return nats.subscribe(
        std::string(event_traits<currency_pair_convention_changed_event>::name),
        [cache](ores::nats::message msg) {
            using ores::eventing::domain::entity_change_event;
            auto evt = rfl::json::read<entity_change_event>(ores::nats::as_string_view(msg.data));
            if (evt && !evt->tenant_id.empty()) {
                // Offload to a detached thread: load() calls request_sync,
                // which would block the NATS callback thread if called inline.
                std::thread([cache, tid = evt->tenant_id]() { cache->load(tid); }).detach();
            }
        });
}

} // namespace ores::refdata::service::cache

#endif
