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
#ifndef ORES_SYNTHETIC_API_FEEDS_EXPECTED_MARKET_DATA_HPP
#define ORES_SYNTHETIC_API_FEEDS_EXPECTED_MARKET_DATA_HPP

#include "ores.marketdata.api/messaging/party_demand_operations_protocol.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include "ores.synthetic.api/domain/fx_vol_contributor.hpp"
#include "ores.synthetic.api/domain/fx_vol_surface_feed.hpp"
#include "ores.synthetic.api/domain/fx_vol_surface_feed_contributor.hpp"
#include "ores.synthetic.api/domain/fx_vol_surface_feed_expiry.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/domain/ir_curve_template_entry.hpp"
#include "ores.synthetic.api/domain/market_data_generation_config.hpp"
#include "ores.synthetic.api/export.hpp"
#include <set>
#include <string>
#include <vector>

namespace ores::synthetic::feed {

/**
 * @brief What one generator publishes, and the providers it publishes for.
 *
 * A spot or a curve publishes for the provider of its collection. A surface publishes for
 * the provider of each of its contributors.
 */
struct generator_publication final {
    std::set<std::string> providers;
    std::vector<std::string> outputs;
};

/**
 * @brief The generator configuration a party can see, as its rows are read.
 */
struct generator_catalogue final {
    std::vector<domain::market_data_generation_config> collections;
    std::vector<domain::ir_curve_generation_config> curves;
    std::vector<domain::ir_curve_template_entry> curve_entries;
    std::vector<domain::fx_spot_generation_config> spots;
    std::vector<domain::fx_vol_surface_feed> surfaces;
    std::vector<domain::fx_vol_surface_feed_expiry> surface_expiries;
    std::vector<domain::fx_vol_contributor> contributors;
    std::vector<domain::fx_vol_surface_feed_contributor> surface_contributors;
};

/**
 * @brief The generators that publish: each enabled config in an enabled collection.
 *
 * A config whose outputs cannot be named, such as a spot whose address is not a spot
 * quote, publishes nothing a demand can name, so it is left out.
 */
ORES_SYNTHETIC_API_EXPORT std::vector<generator_publication>
generator_publications(const generator_catalogue& catalogue);

/**
 * @brief The market data a party is configured to receive, before any of it arrives.
 *
 * A generator counts when it publishes for the provider of an entitled route of the party
 * and one of its outputs is an item that route demands. The set is every output of the
 * generators that count: a curve demanded for its quotes also states its index's fixings,
 * which no route names.
 *
 * @param routes The party's routes, as the market data service computes their demand.
 * @param generators Every generator the party can see.
 */
ORES_SYNTHETIC_API_EXPORT std::set<std::string>
expected_market_data(const std::vector<ores::marketdata::messaging::demanded_route>& routes,
                     const std::vector<generator_publication>& generators);

}

#endif
