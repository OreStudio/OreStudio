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
#ifndef ORES_ORE_CORE_DOMAIN_TODAYS_MARKET_MAPPER_HPP
#define ORES_ORE_CORE_DOMAIN_TODAYS_MARKET_MAPPER_HPP

#include "ores.analytics.api/domain/todays_market_collection.hpp"
#include "ores.analytics.api/domain/todays_market_config.hpp"
#include "ores.analytics.api/domain/todays_market_configuration.hpp"
#include "ores.analytics.api/domain/todays_market_configuration_binding.hpp"
#include "ores.analytics.api/domain/todays_market_entry.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include <vector>

namespace ores::ore::domain {

/**
 * @brief One ORE TodaysMarket document, mapped to the analytics entities.
 *
 * Five tables, not one per collection. A collection is a thing with its own
 * optional id and a position, so it has a table; the twenty-four collections
 * are the same shape, so they share it. An entry belongs to one collection.
 * A configuration is a bundle of references rather than a list of entries, so
 * it and its bindings have tables of their own.
 */
struct mapped_todays_market {
    analytics::domain::todays_market_config config;
    std::vector<analytics::domain::todays_market_collection> collections;
    std::vector<analytics::domain::todays_market_entry> entries;
    std::vector<analytics::domain::todays_market_configuration> configurations;
    std::vector<analytics::domain::todays_market_configuration_binding> bindings;
};

/**
 * @brief Maps between an ORE TodaysMarket XML document and the analytics
 * today's market entities.
 *
 * The binding is regular enough that the twenty-four collections are four
 * shapes rather than twenty-four: nineteen entries keyed by a plain =name=,
 * four keyed by an attribute with another name, two keyed twice by an optional
 * key and an enum currency, and =SwapIndexCurves=, whose entry has no reference
 * text because its value is its nested =Discounting= child.
 */
class ORES_ORE_CORE_EXPORT todays_market_mapper {
public:
    /**
     * @brief Maps an ORE TodaysMarket document to the analytics entities.
     */
    static mapped_todays_market map(const todaysmarket& v);

    /**
     * @brief Reconstructs an ORE TodaysMarket document from mapped entities.
     *
     * Every collection and every configuration is emitted in =position= order,
     * and so is every entry within its collection, because a collection may
     * name the same key twice and a reference cannot be sorted back into
     * document order.
     */
    static todaysmarket reverse(const mapped_todays_market& v);
};

}

#endif
