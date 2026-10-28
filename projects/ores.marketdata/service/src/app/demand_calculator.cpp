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
#include "ores.marketdata.service/app/demand_calculator.hpp"
#include "ores.marketdata.core/terms/provider_terms.hpp"
#include <algorithm>
#include <set>
#include <tuple>

namespace ores::marketdata::service::app {

namespace {

bool holds(const domain::market_data_mapping& mapping, std::chrono::system_clock::time_point now) {
    return mapping.effective_from <= now && (!mapping.effective_to || now < *mapping.effective_to);
}

bool has_a_right(const domain::subscription& s) {
    return s.display || s.non_display || s.derived;
}

bool before(const ores::marketdata::messaging::vendor_id& a,
            const ores::marketdata::messaging::vendor_id& b) {
    return std::tie(a.id_scheme, a.external_id, a.field) <
           std::tie(b.id_scheme, b.external_id, b.field);
}

bool same(const ores::marketdata::messaging::vendor_id& a,
          const ores::marketdata::messaging::vendor_id& b) {
    return std::tie(a.id_scheme, a.external_id, a.field) ==
           std::tie(b.id_scheme, b.external_id, b.field);
}

}

std::vector<party_demand> compute_party_demand(const demand_catalogue& catalogue,
                                               std::chrono::system_clock::time_point now,
                                               const demand_policy& policy) {
    // The vendor ids of each provider, by the item that a mapping that holds names.
    std::map<std::string,
             std::map<std::string, std::vector<ores::marketdata::messaging::vendor_id>>>
        by_item;
    for (const auto& mapping : catalogue.mappings) {
        if (!holds(mapping, now))
            continue;
        const auto instrument = std::ranges::find(catalogue.instruments,
                                                  mapping.vendor_instrument_id,
                                                  &domain::market_data_vendor_instrument::id);
        if (instrument == catalogue.instruments.end())
            continue;
        by_item[instrument->provider_code][mapping.oresmd_uri].push_back(
            {instrument->id_scheme, instrument->external_id, instrument->field});
    }

    std::vector<party_demand> out;
    for (const auto& route : catalogue.routes) {
        const auto source =
            std::ranges::find(catalogue.sources, route.source_id, &domain::market_data_source::id);
        if (source == catalogue.sources.end())
            continue;

        party_demand demand;
        demand.tenant_id = route.tenant_id;
        demand.party_id = route.party_id;
        demand.provider_code = route.provider_code;
        demand.source_id = route.source_id;
        demand.adapter_id = source->name;
        const auto provider =
            std::ranges::find(catalogue.providers, route.provider_code, &domain::provider::code);
        const bool subscribed =
            std::ranges::any_of(catalogue.subscriptions, [&](const domain::subscription& s) {
                return s.tenant_id == route.tenant_id && s.party_id == route.party_id &&
                       s.provider_code == route.provider_code && has_a_right(s);
            });
        if (provider != catalogue.providers.end() && !terms::may_fetch(*provider))
            demand.refusal = "terms_not_accepted";
        else if (!subscribed)
            demand.refusal = "not_entitled";
        demand.entitled = demand.refusal.empty();
        if (demand.entitled) {
            const auto items = by_item.find(route.provider_code);
            if (items != by_item.end()) {
                std::set<std::string> wanted;
                const auto restricted =
                    policy.requirements ? policy.requirements(route.party_id, route.provider_code) :
                                          std::nullopt;
                if (restricted) {
                    for (const auto& uri : *restricted) {
                        wanted.insert(uri);
                        if (policy.composite_inputs)
                            for (const auto& input : policy.composite_inputs(uri))
                                wanted.insert(input);
                    }
                }
                for (const auto& [uri, ids] : items->second) {
                    if (!restricted || wanted.contains(uri))
                        demand.items.emplace(uri, ids);
                }
            }
        }
        out.push_back(std::move(demand));
    }
    return out;
}

demand_result compute_demand(const demand_catalogue& catalogue,
                             std::chrono::system_clock::time_point now,
                             const demand_policy& policy) {
    demand_result result;
    for (const auto& demand : compute_party_demand(catalogue, now, policy)) {
        auto& demanded = result.by_adapter[demand.adapter_id];
        if (!demand.entitled) {
            result.refused.push_back({demand.adapter_id, demand.party_id, demand.refusal});
            continue;
        }
        for (const auto& [uri, ids] : demand.items)
            demanded.insert(demanded.end(), ids.begin(), ids.end());
    }

    for (auto& [adapter, ids] : result.by_adapter) {
        std::ranges::sort(ids, before);
        ids.erase(std::unique(ids.begin(), ids.end(), same), ids.end());
    }
    return result;
}

}
