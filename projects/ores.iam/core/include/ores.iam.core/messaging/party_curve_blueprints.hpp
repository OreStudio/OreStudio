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
#ifndef ORES_IAM_MESSAGING_PARTY_CURVE_BLUEPRINTS_HPP
#define ORES_IAM_MESSAGING_PARTY_CURVE_BLUEPRINTS_HPP

#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_config.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_pillar.hpp"
#include <boost/uuid/name_generator_sha1.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace ores::iam::messaging {

/// The system's curve blueprints, as the party step reads them from the system tenant.
struct system_curve_blueprints {
    std::vector<ores::refdata::domain::ir_curve_bootstrap_config> configs;
    std::vector<ores::refdata::domain::ir_curve_bootstrap_pillar> pillars;
    /// Every series a config or a pillar names: the outputs and the quotes.
    std::vector<ores::marketdata::domain::market_series> series;
};

/// What one party already holds that a copy of the blueprints depends on.
struct party_curve_holdings {
    boost::uuids::uuid party_id;
    /// The party's own series. A pillar reads the series of the same item and provider.
    std::vector<ores::marketdata::domain::market_series> series;
    /// The ids of the curve configs the party already holds.
    std::set<boost::uuids::uuid> config_ids;
};

/// A curve the party can bootstrap, by its config id and name.
struct party_curve {
    boost::uuids::uuid config_id;
    std::string name;
};

/**
 * @brief The rows that give one party its own copy of the system's curve blueprints.
 *
 * Only rows the party does not hold yet are written, so a second run writes nothing. A config
 * whose quotes have not reached the party is waiting, with the series it lacks named, and none of
 * its rows are written.
 */
struct party_curve_plan {
    std::vector<ores::marketdata::domain::market_series> series_to_write;
    std::vector<ores::refdata::domain::ir_curve_bootstrap_config> configs_to_write;
    std::vector<ores::refdata::domain::ir_curve_bootstrap_pillar> pillars_to_write;
    /// The configs the party holds once the rows above are written, in the system's order.
    std::vector<party_curve> ready;
    /// One line for each config that waits, naming the curve and the series it lacks.
    std::vector<std::string> waiting;
};

/**
 * @brief The id of a party's copy of a system row.
 *
 * The id is derived from the party and the system row, so every run names the same copy.
 */
[[nodiscard]] inline boost::uuids::uuid party_copy_id(const boost::uuids::uuid& party_id,
                                                      std::string_view kind,
                                                      const boost::uuids::uuid& system_id) {
    return boost::uuids::name_generator_sha1{party_id}(std::string(kind) + ":" +
                                                       boost::uuids::to_string(system_id));
}

/**
 * @brief Plans one party's copy of the system's curve blueprints.
 *
 * A curve is derived data, so each party bootstraps its own from the raw quotes its
 * subscriptions deliver into its own series. The copy keeps the system's conventions and pillars.
 * Each pillar reads the party's series of the same item from the same provider, and each config
 * writes its own output series under the party.
 */
[[nodiscard]] inline party_curve_plan plan_party_curves(const system_curve_blueprints& system,
                                                        const party_curve_holdings& party) {
    std::map<boost::uuids::uuid, const ores::marketdata::domain::market_series*> system_series;
    for (const auto& s : system.series)
        system_series.emplace(s.id, &s);

    std::map<std::pair<std::string, std::string>, boost::uuids::uuid> own_series;
    std::set<boost::uuids::uuid> own_ids;
    for (const auto& s : party.series) {
        if (s.party_id != party.party_id)
            continue;
        own_series.emplace(std::pair{s.oresmd_uri, s.provider_code}, s.id);
        own_ids.insert(s.id);
    }

    std::map<boost::uuids::uuid, std::vector<const ores::refdata::domain::ir_curve_bootstrap_pillar*>>
        pillars_of;
    for (const auto& p : system.pillars)
        pillars_of[p.bootstrap_config_id].push_back(&p);

    party_curve_plan plan;
    for (const auto& config : system.configs) {
        const auto config_id = party_copy_id(party.party_id, "ir_curve_bootstrap_config", config.id);
        if (party.config_ids.contains(config_id)) {
            plan.ready.push_back({config_id, config.name});
            continue;
        }

        const auto output = system_series.find(config.output_series_id);
        if (output == system_series.end()) {
            plan.waiting.push_back(config.name + ": the system holds no output series " +
                                   boost::uuids::to_string(config.output_series_id));
            continue;
        }

        std::vector<ores::refdata::domain::ir_curve_bootstrap_pillar> pillars;
        std::vector<std::string> lacking;
        for (const auto* p : pillars_of[config.id]) {
            const auto quotes = system_series.find(p->market_series_id);
            if (quotes == system_series.end()) {
                lacking.push_back("system series " + boost::uuids::to_string(p->market_series_id));
                continue;
            }
            const auto own = own_series.find(
                {quotes->second->oresmd_uri, quotes->second->provider_code});
            if (own == own_series.end()) {
                lacking.push_back(quotes->second->provider_code + " " +
                                  quotes->second->oresmd_uri);
                continue;
            }
            auto copy = *p;
            copy.version = 0;
            copy.id = party_copy_id(party.party_id, "ir_curve_bootstrap_pillar", p->id);
            copy.party_id = party.party_id;
            copy.bootstrap_config_id = config_id;
            copy.market_series_id = own->second;
            pillars.push_back(std::move(copy));
        }
        if (!lacking.empty()) {
            std::set<std::string> distinct(lacking.begin(), lacking.end());
            std::string named;
            for (const auto& l : distinct)
                named += (named.empty() ? "" : ", ") + l;
            plan.waiting.push_back(config.name + ": no series of " + named);
            continue;
        }

        auto series = *output->second;
        series.version = 0;
        series.id = party_copy_id(party.party_id, "market_series", series.id);
        series.party_id = party.party_id;
        series.derivation_kind = "OBSERVED";
        series.derivation_config_id = boost::uuids::nil_uuid();
        series.derivation_config_version = 0;
        if (!own_ids.contains(series.id))
            plan.series_to_write.push_back(std::move(series));

        auto copy = config;
        copy.version = 0;
        copy.id = config_id;
        copy.party_id = party.party_id;
        copy.output_series_id = party_copy_id(party.party_id, "market_series", config.output_series_id);
        if (config.discount_curve_config_id)
            copy.discount_curve_config_id =
                party_copy_id(party.party_id, "ir_curve_bootstrap_config",
                              *config.discount_curve_config_id);
        plan.configs_to_write.push_back(std::move(copy));
        for (auto& p : pillars)
            plan.pillars_to_write.push_back(std::move(p));
        plan.ready.push_back({config_id, config.name});
    }

    // A curve discounted off another can be written only once that curve exists. Dropping one
    // config can strand another that discounts off it, so the pass repeats until nothing moves.
    for (bool dropped = true; dropped;) {
        dropped = false;
        std::set<boost::uuids::uuid> available(party.config_ids);
        for (const auto& c : plan.configs_to_write)
            available.insert(c.id);
        for (auto it = plan.configs_to_write.begin(); it != plan.configs_to_write.end();) {
            if (!it->discount_curve_config_id ||
                available.contains(*it->discount_curve_config_id)) {
                ++it;
                continue;
            }
            const auto id = it->id;
            const auto output = it->output_series_id;
            std::erase_if(plan.pillars_to_write,
                          [&](const auto& p) { return p.bootstrap_config_id == id; });
            std::erase_if(plan.series_to_write, [&](const auto& s) { return s.id == output; });
            std::erase_if(plan.ready, [&](const auto& r) { return r.config_id == id; });
            plan.waiting.push_back(it->name + ": its discount curve is not written yet");
            it = plan.configs_to_write.erase(it);
            dropped = true;
        }
    }
    return plan;
}

}

#endif
