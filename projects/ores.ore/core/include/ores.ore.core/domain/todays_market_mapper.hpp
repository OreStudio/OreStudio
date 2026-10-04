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
#include "ores.analytics.api/domain/todays_market_document.hpp"
#include "ores.analytics.api/domain/todays_market_entry.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include <vector>

namespace ores::ore::domain {

/**
 * @brief Maps between an ORE TodaysMarket XML document and the analytics
 * today's market entities.
 *
 * The binding is regular enough that the twenty-four collections are four
 * shapes rather than twenty-four: eighteen entries keyed by a plain =name=,
 * three keyed by another attribute (=currency= or =pair=), two keyed twice by
 * an optional =key= and an optional enum =currency=, and =SwapIndexCurves=,
 * keyed by =name= but with no reference text because its value is its nested
 * =Discounting= child.
 *
 * A collection name or a binding name that is not one of the twenty-four is an
 * error on the way back, not a skip, so a bad row cannot pass as a round trip.
 */
class ORES_ORE_CORE_EXPORT todays_market_mapper {
public:
    /**
     * @brief Maps an ORE TodaysMarket document to the analytics entities.
     */
    static ores::analytics::domain::todays_market_document map(const todaysmarket& v);

    /**
     * @brief Reconstructs an ORE TodaysMarket document from mapped entities.
     *
     * Every collection and every configuration is emitted in =position= order,
     * and so is every entry within its collection, because a collection may
     * name the same key twice and a reference cannot be sorted back into
     * document order.
     */
    static todaysmarket reverse(const ores::analytics::domain::todays_market_document& v);
};

}

#endif
