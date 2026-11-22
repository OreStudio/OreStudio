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
#include "ores.synthetic.api/feeds/expected_market_data.hpp"
#include "ores.synthetic.api/feeds/generator_outputs.hpp"
#include <algorithm>
#include <map>

namespace ores::synthetic::feed {

std::vector<generator_publication> generator_publications(const generator_catalogue& catalogue) {
    std::map<boost::uuids::uuid, std::string> providers;
    for (const auto& c : catalogue.collections)
        if (c.enabled)
            providers.emplace(c.id, c.provider_code);
    const auto provider_of = [&providers](const boost::uuids::uuid& collection) {
        const auto found = providers.find(collection);
        return found == providers.end() ? std::set<std::string>{} :
                                          std::set<std::string>{found->second};
    };

    std::vector<generator_publication> out;
    for (const auto& curve : catalogue.curves) {
        if (!curve.enabled)
            continue;
        auto outputs = ir_curve_outputs(curve, catalogue.curve_entries);
        if (outputs && !outputs->empty())
            out.push_back(
                {.providers = provider_of(curve.config_id), .outputs = std::move(*outputs)});
    }
    for (const auto& spot : catalogue.spots) {
        if (!spot.enabled)
            continue;
        if (auto quote = fx_spot_quote_uri(spot.oresmd_uri))
            out.push_back(
                {.providers = provider_of(spot.config_id), .outputs = {std::move(*quote)}});
    }

    std::map<boost::uuids::uuid, std::string> contributor_providers;
    for (const auto& c : catalogue.contributors)
        contributor_providers.emplace(c.id, c.provider_code);
    for (const auto& surface : catalogue.surfaces) {
        if (!surface.enabled || !providers.contains(surface.config_id))
            continue;
        generator_publication one;
        for (const auto& link : catalogue.surface_contributors)
            if (link.feed_id == surface.id)
                if (const auto found = contributor_providers.find(link.contributor_id);
                    found != contributor_providers.end())
                    one.providers.insert(found->second);
        std::vector<std::string> expiries;
        for (const auto& e : catalogue.surface_expiries)
            if (e.feed_id == surface.id)
                expiries.push_back(e.tenor);
        one.outputs = fx_vol_surface_outputs(
            surface.base_currency_code, surface.quote_currency_code, expiries);
        if (!one.outputs.empty())
            out.push_back(std::move(one));
    }
    return out;
}

std::set<std::string>
expected_market_data(const std::vector<ores::marketdata::messaging::demanded_route>& routes,
                     const std::vector<generator_publication>& generators) {
    std::set<std::string> out;
    for (const auto& generator : generators) {
        const bool counts = std::ranges::any_of(routes, [&generator](const auto& route) {
            return route.entitled && generator.providers.contains(route.provider_code) &&
                   std::ranges::any_of(generator.outputs, [&route](const std::string& output) {
                       return std::ranges::binary_search(route.items, output);
                   });
        });
        if (counts)
            out.insert(generator.outputs.begin(), generator.outputs.end());
    }
    return out;
}

}
