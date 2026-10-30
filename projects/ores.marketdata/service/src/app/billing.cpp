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
#include "ores.marketdata.service/app/billing.hpp"
#include <algorithm>
#include <map>
#include <numeric>
#include <set>
#include <tuple>

namespace ores::marketdata::service::app {

namespace {

using party_key = std::pair<boost::uuids::uuid, boost::uuids::uuid>;

party_key key_of(const ores::utility::uuid::tenant_id& tenant, const boost::uuids::uuid& party) {
    return {tenant.to_uuid(), party};
}

bool overlaps(std::chrono::system_clock::time_point from,
              const std::optional<std::chrono::system_clock::time_point>& to,
              const billing_period& p) {
    return from < p.to && (!to || *to > p.from);
}

bool applies(const domain::rate_card_component& c,
             const boost::uuids::uuid& source,
             const billing_period& p) {
    return c.feed_id == source && c.effective_from < p.to &&
           (!c.effective_to || *c.effective_to > p.from);
}

/// The events of one source in one period, as the sets that the drivers count.
struct counted {
    std::set<party_key> subscribers;
    std::map<std::string, std::set<party_key>> holders;
    std::set<std::string> instruments;
    std::map<std::string, std::set<party_key>> demanders;
    std::map<party_key, std::int64_t> ticks;
    std::int64_t total_ticks = 0;
};

counted count_events(const boost::uuids::uuid& source,
                     const usage_events& events,
                     const billing_period& p) {
    counted c;
    for (const auto& i : events.subscriptions) {
        if (i.feed_id != source || !overlaps(i.interval_from, i.interval_to, p))
            continue;
        const auto party = key_of(i.tenant_id, i.party_id);
        c.subscribers.insert(party);
        if (i.non_display)
            c.holders["non_display"].insert(party);
        if (i.derived)
            c.holders["derived"].insert(party);
    }
    for (const auto& d : events.demands) {
        if (d.feed_id != source || !overlaps(d.interval_from, d.interval_to, p))
            continue;
        c.instruments.insert(d.oresmd_uri);
        c.demanders[d.oresmd_uri].insert(key_of(d.tenant_id, d.party_id));
    }
    for (const auto& n : events.deliveries) {
        if (n.feed_id != source || n.period_start < p.from || n.period_start >= p.to)
            continue;
        c.ticks[key_of(n.tenant_id, n.party_id)] += n.deliveries;
        c.total_ticks += n.deliveries;
    }
    return c;
}

std::int64_t per_thousand(std::int64_t ticks, std::int64_t charge) {
    return static_cast<std::int64_t>((static_cast<__int128>(ticks) * charge + 500) / 1000);
}

}

std::vector<std::int64_t> split_evenly(std::int64_t total, std::size_t parts) {
    std::vector<std::int64_t> out(parts, 0);
    if (parts == 0)
        return out;
    const auto n = static_cast<std::int64_t>(parts);
    const auto base = total / n;
    const auto extra = total % n;
    for (std::int64_t i = 0; i < n; ++i)
        out[static_cast<std::size_t>(i)] = base + (i < extra ? 1 : 0);
    return out;
}

std::vector<std::int64_t> split_weighted(std::int64_t total,
                                         const std::vector<std::int64_t>& weights) {
    std::vector<std::int64_t> out(weights.size(), 0);
    const auto sum = std::accumulate(weights.begin(), weights.end(), std::int64_t{0});
    if (sum <= 0 || total == 0)
        return out;

    std::vector<std::pair<__int128, std::size_t>> fractions;
    std::int64_t given = 0;
    for (std::size_t i = 0; i < weights.size(); ++i) {
        const __int128 scaled = static_cast<__int128>(total) * weights[i];
        out[i] = static_cast<std::int64_t>(scaled / sum);
        given += out[i];
        fractions.push_back({scaled % sum, i});
    }
    std::stable_sort(fractions.begin(), fractions.end(), [](const auto& a, const auto& b) {
        return a.first > b.first;
    });
    for (std::int64_t left = total - given, k = 0; left > 0; --left, ++k)
        ++out[fractions[static_cast<std::size_t>(k) % fractions.size()].second];
    return out;
}

bill_report make_report(const std::vector<domain::feed>& sources,
                        const std::vector<domain::rate_card_component>& card,
                        const usage_events& events,
                        const billing_period& period,
                        const std::string& source_name) {
    bill_report report;
    for (const auto& source : sources) {
        if (!source_name.empty() && source.name != source_name)
            continue;
        auto bill = rate_source(source, card, events, period);
        if (bill.lines.empty())
            continue;
        auto split = allocate(bill, events);
        report.unallocated_micros += split.unallocated_micros;
        for (auto& c : split.charges)
            report.charges.push_back(std::move(c));
        report.bills.push_back(std::move(bill));
    }
    return report;
}

source_bill rate_source(const domain::feed& source,
                        const std::vector<domain::rate_card_component>& card,
                        const usage_events& events,
                        const billing_period& period) {
    source_bill bill;
    bill.feed_id = source.id;
    bill.source_name = source.name;
    bill.shadow = source.kind == "simulated";
    bill.period = period;

    const auto c = count_events(source.id, events, period);

    // Of the components that share a driver and a licence right and apply in the period, the one
    // that started last holds, so a new contract that did not close the old row bills once.
    std::map<std::pair<std::string, std::string>, const domain::rate_card_component*> held;
    std::vector<const domain::rate_card_component*> order;
    for (const auto& component : card) {
        if (!applies(component, source.id, period))
            continue;
        const auto key = std::make_pair(component.driver, component.licence_right);
        const auto found = held.find(key);
        if (found == held.end()) {
            held.emplace(key, &component);
            order.push_back(&component);
        } else if (component.effective_from > found->second->effective_from) {
            std::ranges::replace(order, found->second, &component);
            found->second = &component;
        }
    }

    for (const auto* current : order) {
        const auto& component = *current;
        bill_line line;
        line.driver = component.driver;
        line.licence_right = component.licence_right;
        line.charge_micros = component.charge_micros;
        if (component.driver == "fixed_per_period") {
            line.quantity = 1;
        } else if (component.driver == "per_instrument") {
            line.quantity = static_cast<std::int64_t>(c.instruments.size());
        } else if (component.driver == "per_party") {
            line.quantity = static_cast<std::int64_t>(c.subscribers.size());
        } else if (component.driver == "per_licence") {
            const auto held = c.holders.find(component.licence_right);
            line.quantity =
                held == c.holders.end() ? 0 : static_cast<std::int64_t>(held->second.size());
        } else if (component.driver == "per_thousand_ticks") {
            line.quantity = c.total_ticks;
            line.total_micros = per_thousand(c.total_ticks, component.charge_micros);
        } else {
            // The entity refuses any other driver, so this is a driver a later change added
            // to the table and not to the rating. It has no line, and no currency.
            continue;
        }
        if (bill.currency.empty())
            bill.currency = component.currency;
        if (component.driver != "per_thousand_ticks")
            line.total_micros = line.quantity * component.charge_micros;
        bill.total_micros += line.total_micros;
        bill.lines.push_back(std::move(line));
    }
    return bill;
}

allocation allocate(const source_bill& bill, const usage_events& events) {
    allocation result;
    const auto c = count_events(bill.feed_id, events, bill.period);

    // The tenant of a party, which the key keeps as a uuid.
    std::map<party_key, ores::utility::uuid::tenant_id> tenants;
    for (const auto& i : events.subscriptions)
        tenants.emplace(key_of(i.tenant_id, i.party_id), i.tenant_id);
    for (const auto& d : events.demands)
        tenants.emplace(key_of(d.tenant_id, d.party_id), d.tenant_id);
    for (const auto& n : events.deliveries)
        tenants.emplace(key_of(n.tenant_id, n.party_id), n.tenant_id);

    std::map<std::tuple<party_key, std::string, std::string>, std::int64_t> shares;
    const auto give = [&](const party_key& party,
                          const std::string& driver,
                          const std::string& right,
                          std::int64_t amount) {
        if (amount != 0)
            shares[{party, driver, right}] += amount;
    };

    for (const auto& line : bill.lines) {
        const std::vector<party_key> subscribers(c.subscribers.begin(), c.subscribers.end());
        std::int64_t placed = 0;
        const auto place = [&](const party_key& party, std::int64_t amount) {
            give(party, line.driver, line.licence_right, amount);
            placed += amount;
        };

        if (line.driver == "fixed_per_period") {
            const auto parts = split_evenly(line.total_micros, subscribers.size());
            for (std::size_t i = 0; i < subscribers.size(); ++i)
                place(subscribers[i], parts[i]);
        } else if (line.driver == "per_party") {
            for (const auto& party : subscribers)
                place(party, line.charge_micros);
        } else if (line.driver == "per_licence") {
            const auto held = c.holders.find(line.licence_right);
            if (held != c.holders.end())
                for (const auto& party : held->second)
                    place(party, line.charge_micros);
        } else if (line.driver == "per_instrument") {
            for (const auto& [uri, demanders] : c.demanders) {
                const std::vector<party_key> parties(demanders.begin(), demanders.end());
                const auto parts = split_evenly(line.charge_micros, parties.size());
                for (std::size_t i = 0; i < parties.size(); ++i)
                    place(parties[i], parts[i]);
            }
        } else if (line.driver == "per_thousand_ticks") {
            std::vector<party_key> parties;
            std::vector<std::int64_t> weights;
            for (const auto& [party, ticks] : c.ticks) {
                parties.push_back(party);
                weights.push_back(ticks);
            }
            const auto parts = split_weighted(line.total_micros, weights);
            for (std::size_t i = 0; i < parties.size(); ++i)
                place(parties[i], parts[i]);
        }
        result.unallocated_micros += line.total_micros - placed;
    }

    for (const auto& [k, amount] : shares) {
        const auto& [party, driver, right] = k;
        party_charge charge;
        charge.tenant_id = tenants.at(party);
        charge.party_id = party.second;
        charge.feed_id = bill.feed_id;
        charge.driver = driver;
        charge.licence_right = right;
        charge.total_micros = amount;
        result.charges.push_back(std::move(charge));
    }
    return result;
}

}
