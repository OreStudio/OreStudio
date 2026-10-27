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
#include "ores.marketdata.service/app/usage_planner.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <set>
#include <tuple>

namespace ores::marketdata::service::app {

namespace {

constexpr auto opened_reason = "system.new_record";
constexpr auto closed_reason = "system.update";

template <typename Interval>
void stamp(Interval& i,
           const std::string& actor,
           const char* reason,
           const std::string& commentary) {
    i.modified_by = actor;
    i.performed_by = actor;
    i.change_reason_code = reason;
    i.change_commentary = commentary;
}

auto usage_planner_key_of(const subscription_state& s) {
    return std::make_tuple(s.tenant_id.to_string(),
                           s.party_id,
                           s.provider_code,
                           s.source_id,
                           s.display,
                           s.non_display,
                           s.derived);
}

auto usage_planner_key_of(const domain::subscription_interval& i) {
    return std::make_tuple(i.tenant_id.to_string(),
                           i.party_id,
                           i.provider_code,
                           i.source_id,
                           i.display,
                           i.non_display,
                           i.derived);
}

auto usage_planner_key_of(const demand_state& s) {
    return std::make_tuple(
        s.tenant_id.to_uuid(), s.party_id, s.provider_code, s.source_id, s.oresmd_uri);
}

auto usage_planner_key_of(const domain::demand_interval& i) {
    return std::make_tuple(
        i.tenant_id.to_uuid(), i.party_id, i.provider_code, i.source_id, i.oresmd_uri);
}

}

std::vector<subscription_state> subscription_states(const demand_catalogue& catalogue) {
    std::vector<subscription_state> out;
    for (const auto& route : catalogue.routes) {
        for (const auto& s : catalogue.subscriptions) {
            if (s.tenant_id != route.tenant_id || s.party_id != route.party_id ||
                s.provider_code != route.provider_code)
                continue;
            if (!(s.display || s.non_display || s.derived))
                continue;
            out.push_back({route.tenant_id,
                           route.party_id,
                           route.provider_code,
                           route.source_id,
                           s.display,
                           s.non_display,
                           s.derived});
            break;
        }
    }
    return out;
}

std::vector<demand_state> demand_states(const std::vector<party_demand>& demand) {
    std::vector<demand_state> out;
    for (const auto& d : demand) {
        if (!d.entitled)
            continue;
        for (const auto& [uri, ids] : d.items)
            out.push_back({d.tenant_id, d.party_id, d.provider_code, d.source_id, uri});
    }
    return out;
}

interval_plan<domain::subscription_interval>
plan_subscription_intervals(const std::vector<subscription_state>& now_state,
                            const std::vector<domain::subscription_interval>& open,
                            std::chrono::system_clock::time_point now,
                            const std::string& actor) {
    interval_plan<domain::subscription_interval> plan;
    boost::uuids::random_generator gen;

    std::set<decltype(usage_planner_key_of(subscription_state{}))> wanted;
    for (const auto& s : now_state)
        wanted.insert(usage_planner_key_of(s));
    std::set<decltype(usage_planner_key_of(domain::subscription_interval{}))> held;
    for (const auto& i : open) {
        if (i.interval_to)
            continue;
        const auto key = usage_planner_key_of(i);
        if (wanted.contains(key)) {
            held.insert(key);
        } else {
            auto closed = i;
            closed.interval_to = now;
            stamp(closed, actor, closed_reason, "The route or the subscription ended or changed.");
            plan.to_close.push_back(std::move(closed));
        }
    }
    for (const auto& s : now_state) {
        if (held.contains(usage_planner_key_of(s)))
            continue;
        domain::subscription_interval i;
        i.id = gen();
        i.tenant_id = s.tenant_id;
        i.party_id = s.party_id;
        i.provider_code = s.provider_code;
        i.source_id = s.source_id;
        i.display = s.display;
        i.non_display = s.non_display;
        i.derived = s.derived;
        i.interval_from = now;
        stamp(i,
              actor,
              opened_reason,
              "The party holds a route and a subscription with these rights.");
        plan.to_open.push_back(std::move(i));
        held.insert(usage_planner_key_of(s));
    }
    return plan;
}

interval_plan<domain::demand_interval>
plan_demand_intervals(const std::vector<demand_state>& now_state,
                      const std::vector<domain::demand_interval>& open,
                      std::chrono::system_clock::time_point now,
                      const std::string& actor) {
    interval_plan<domain::demand_interval> plan;
    boost::uuids::random_generator gen;

    std::set<decltype(usage_planner_key_of(demand_state{}))> wanted;
    for (const auto& s : now_state)
        wanted.insert(usage_planner_key_of(s));
    std::set<decltype(usage_planner_key_of(domain::demand_interval{}))> held;
    for (const auto& i : open) {
        if (i.interval_to)
            continue;
        const auto key = usage_planner_key_of(i);
        if (wanted.contains(key)) {
            held.insert(key);
        } else {
            auto closed = i;
            closed.interval_to = now;
            stamp(closed, actor, closed_reason, "The item left the demand of the party.");
            plan.to_close.push_back(std::move(closed));
        }
    }
    for (const auto& s : now_state) {
        if (held.contains(usage_planner_key_of(s)))
            continue;
        domain::demand_interval i;
        i.id = gen();
        i.tenant_id = s.tenant_id;
        i.party_id = s.party_id;
        i.provider_code = s.provider_code;
        i.source_id = s.source_id;
        i.oresmd_uri = s.oresmd_uri;
        i.interval_from = now;
        stamp(i, actor, opened_reason, "The item entered the demand of the party.");
        plan.to_open.push_back(std::move(i));
        held.insert(usage_planner_key_of(s));
    }
    return plan;
}
}
