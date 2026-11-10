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

    // The items of each scoped feed. A publisher with two access routes, such as Frankfurter
    // for its FX rates and its own portal for its overnight rate, scopes each feed to the items
    // its adapter reads.
    std::map<boost::uuids::uuid, std::set<std::string>> scoped;
    for (const auto& scope : catalogue.scopes)
        scoped[scope.feed_id].insert(scope.item);

    std::vector<party_demand> out;
    for (const auto& route : catalogue.routes) {
        const auto source = std::ranges::find(catalogue.feeds, route.feed_id, &domain::feed::id);
        if (source == catalogue.feeds.end())
            continue;

        party_demand demand;
        demand.tenant_id = route.tenant_id;
        demand.party_id = route.party_id;
        demand.provider_code = route.publisher_code;
        demand.feed_id = route.feed_id;
        demand.adapter_id = source->name;
        const auto provider =
            std::ranges::find(catalogue.providers, route.publisher_code, &domain::provider::code);
        const bool subscribed =
            std::ranges::any_of(catalogue.subscriptions, [&](const domain::subscription& s) {
                return s.tenant_id == route.tenant_id && s.party_id == route.party_id &&
                       s.provider_code == route.publisher_code && has_a_right(s);
            });
        // A provider the catalogue does not hold states no terms to meet; its routes still
        // need a subscription.
        if (provider != catalogue.providers.end() && !terms::may_fetch(*provider))
            demand.refusal = "terms_not_accepted";
        else if (!subscribed)
            demand.refusal = "not_entitled";
        demand.entitled = demand.refusal.empty();
        if (demand.entitled) {
            const auto items = by_item.find(route.publisher_code);
            if (items != by_item.end()) {
                std::set<std::string> wanted;
                const auto restricted =
                    policy.requirements ?
                        policy.requirements(route.party_id, route.publisher_code) :
                        std::nullopt;
                if (restricted) {
                    for (const auto& uri : *restricted) {
                        wanted.insert(uri);
                        if (policy.composite_inputs)
                            for (const auto& input : policy.composite_inputs(uri))
                                wanted.insert(input);
                    }
                }
                const auto scope = scoped.find(route.feed_id);
                for (const auto& [uri, ids] : items->second) {
                    if (restricted && !wanted.contains(uri))
                        continue;
                    if (scope != scoped.end() && !scope->second.contains(uri))
                        continue;
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
        auto& served = result.served_by_adapter[demand.adapter_id];
        for (const auto& [uri, ids] : demand.items) {
            demanded.insert(demanded.end(), ids.begin(), ids.end());
            for (const auto& id : ids)
                served.insert({.tenant_id = demand.tenant_id.to_string(),
                               .party_id = demand.party_id,
                               .id_scheme = id.id_scheme,
                               .external_id = id.external_id,
                               .field = id.field});
        }
    }

    for (auto& [adapter, ids] : result.by_adapter) {
        std::ranges::sort(ids, before);
        ids.erase(std::unique(ids.begin(), ids.end(), same), ids.end());
    }
    return result;
}

std::vector<messaging::vendor_id> image_of(const std::set<served_id>& before,
                                           const std::set<served_id>& now) {
    std::set<std::tuple<std::string, std::string, std::string>> seen;
    std::vector<messaging::vendor_id> out;
    for (const auto& s : now) {
        if (before.contains(s))
            continue;
        if (seen.insert({s.id_scheme, s.external_id, s.field}).second)
            out.push_back({s.id_scheme, s.external_id, s.field});
    }
    return out;
}

std::set<served_id> served_after(std::set<served_id> demanded,
                                 const std::vector<messaging::rejected_id>& rejected) {
    std::erase_if(demanded, [&](const served_id& s) {
        return std::ranges::any_of(rejected, [&](const messaging::rejected_id& r) {
            return r.id.id_scheme == s.id_scheme && r.id.external_id == s.external_id &&
                   r.id.field == s.field;
        });
    });
    return demanded;
}

}
