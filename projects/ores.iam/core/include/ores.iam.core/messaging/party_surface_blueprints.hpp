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
#ifndef ORES_IAM_MESSAGING_PARTY_SURFACE_BLUEPRINTS_HPP
#define ORES_IAM_MESSAGING_PARTY_SURFACE_BLUEPRINTS_HPP

#include "ores.iam.core/messaging/party_curve_blueprints.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.refdata.api/domain/fx_vol_surface_blueprint.hpp"
#include "ores.refdata.api/domain/fx_vol_surface_pillar.hpp"
#include "ores.refdata.api/domain/fx_vol_surface_pillar_source.hpp"
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace ores::iam::messaging {

/// The system's surface blueprints, with every series they name.
struct system_surface_blueprints {
    std::vector<ores::refdata::domain::fx_vol_surface_blueprint> blueprints;
    std::vector<ores::refdata::domain::fx_vol_surface_pillar> pillars;
    std::vector<ores::refdata::domain::fx_vol_surface_pillar_source> sources;
    /// The output series and the quote series the sources read.
    std::vector<ores::marketdata::domain::market_series> series;
};

/// What one party already holds that a copy of the surface blueprints depends on.
struct party_surface_holdings {
    boost::uuids::uuid party_id;
    std::vector<ores::marketdata::domain::market_series> series;
    std::set<boost::uuids::uuid> blueprint_ids;
};

/**
 * @brief The rows that give one party its own copy of the system's surface blueprints.
 *
 * Only rows the party does not hold yet are written, so a second run writes nothing. A blueprint
 * whose quotes have not reached the party waits, with the series it lacks named.
 */
struct party_surface_plan {
    std::vector<ores::marketdata::domain::market_series> series_to_write;
    std::vector<ores::refdata::domain::fx_vol_surface_blueprint> blueprints_to_write;
    std::vector<ores::refdata::domain::fx_vol_surface_pillar> pillars_to_write;
    std::vector<ores::refdata::domain::fx_vol_surface_pillar_source> sources_to_write;
    /// The blueprints the party holds once the rows above are written, by name.
    std::vector<std::string> ready;
    std::vector<std::string> waiting;
};

/**
 * @brief Plans one party's copy of the system's surface blueprints.
 *
 * A surface is derived data, so each party calibrates its own from the volatility quotes its
 * subscriptions deliver into its own series. The copy keeps the system's model, expiries and
 * strikes. Each pillar source reads the party's series of the same item from the same provider,
 * and each blueprint writes its own output series under the party.
 */
[[nodiscard]] inline party_surface_plan plan_party_surfaces(const system_surface_blueprints& system,
                                                            const party_surface_holdings& party) {
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

    std::map<boost::uuids::uuid, std::vector<const ores::refdata::domain::fx_vol_surface_pillar*>>
        pillars_of;
    for (const auto& p : system.pillars)
        pillars_of[p.blueprint_id].push_back(&p);
    std::map<boost::uuids::uuid,
             std::vector<const ores::refdata::domain::fx_vol_surface_pillar_source*>>
        sources_of;
    for (const auto& s : system.sources)
        sources_of[s.pillar_id].push_back(&s);

    party_surface_plan plan;
    for (const auto& blueprint : system.blueprints) {
        const auto blueprint_id =
            party_copy_id(party.party_id, "fx_vol_surface_blueprint", blueprint.id);
        if (party.blueprint_ids.contains(blueprint_id)) {
            plan.ready.push_back(blueprint.name);
            continue;
        }
        const auto output = system_series.find(blueprint.output_series_id);
        if (output == system_series.end()) {
            plan.waiting.push_back(blueprint.name + ": the system holds no output series " +
                                   boost::uuids::to_string(blueprint.output_series_id));
            continue;
        }

        std::vector<ores::refdata::domain::fx_vol_surface_pillar> pillars;
        std::vector<ores::refdata::domain::fx_vol_surface_pillar_source> sources;
        std::set<std::string> lacking;
        for (const auto* p : pillars_of[blueprint.id]) {
            auto pillar = *p;
            pillar.version = 0;
            pillar.id = party_copy_id(party.party_id, "fx_vol_surface_pillar", p->id);
            pillar.party_id = party.party_id;
            pillar.blueprint_id = blueprint_id;
            for (const auto* s : sources_of[p->id]) {
                const auto quotes = system_series.find(s->market_series_id);
                if (quotes == system_series.end()) {
                    lacking.insert("system series " + boost::uuids::to_string(s->market_series_id));
                    continue;
                }
                const auto own =
                    own_series.find({quotes->second->oresmd_uri, quotes->second->provider_code});
                if (own == own_series.end()) {
                    lacking.insert(quotes->second->provider_code + " " +
                                   quotes->second->oresmd_uri);
                    continue;
                }
                auto source = *s;
                source.version = 0;
                source.id = party_copy_id(party.party_id, "fx_vol_surface_pillar_source", s->id);
                source.party_id = party.party_id;
                source.pillar_id = pillar.id;
                source.market_series_id = own->second;
                sources.push_back(std::move(source));
            }
            pillars.push_back(std::move(pillar));
        }
        if (!lacking.empty()) {
            std::string named;
            for (const auto& l : lacking)
                named += (named.empty() ? "" : ", ") + l;
            plan.waiting.push_back(blueprint.name + ": no series of " + named);
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

        auto copy = blueprint;
        copy.version = 0;
        copy.id = blueprint_id;
        copy.party_id = party.party_id;
        copy.output_series_id =
            party_copy_id(party.party_id, "market_series", blueprint.output_series_id);
        plan.blueprints_to_write.push_back(std::move(copy));
        for (auto& p : pillars)
            plan.pillars_to_write.push_back(std::move(p));
        for (auto& s : sources)
            plan.sources_to_write.push_back(std::move(s));
        plan.ready.push_back(blueprint.name);
    }
    return plan;
}

}

#endif
