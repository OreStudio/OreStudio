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
#ifndef ORES_ORE_CORE_XML_FORWARDING_CURVES_HPP
#define ORES_ORE_CORE_XML_FORWARDING_CURVES_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/run_package.hpp"
#include <string>
#include <string_view>
#include <vector>

/**
 * @file forwarding_curves.hpp
 * @brief The curves a run's market data publishes, named in its configuration.
 *
 * A party publishes the curves it bootstraps as discount factors, one
 * DISCOUNT/RATE/CCY/NAME/TERM quote for each pillar, and the run's market data
 * carries them. A run's configuration names only the curves it was authored
 * with, so a trade on an index whose curve is published but not named cannot
 * build. This names each such curve: a curve built directly from its discount
 * factors, and the index forwarded on it. It also forwards each term rate the
 * portfolio names, such as GBP-SONIA-3M, on the curve of its overnight index.
 */
namespace ores::ore::xml {

/**
 * @brief A curve the market data publishes as discount factors.
 */
struct published_curve {
    /** The curve's currency, as the quotes state it. */
    std::string currency;
    /** The curve's name, which is the ORE index it projects. */
    std::string name;
    /** The quote keys of its pillars, in key order. */
    std::vector<std::string> quotes;
};

/**
 * @brief The curves a market data file publishes, in name order.
 */
ORES_ORE_CORE_EXPORT std::vector<published_curve> published_curves(std::string_view market_data);

/**
 * @brief The input files, with each published curve the portfolio names
 * forwarded in the today's market and built in the curve configuration, and
 * each term rate the portfolio names forwarded on its overnight index's curve.
 *
 * A curve is added only when the portfolio names its index, the today's
 * market forwards no curve for that index, the curve configuration holds no
 * curve of that id, and the conventions hold a tenor-based zero convention to
 * read its pillars with. A term rate is added only when the today's market
 * forwards its overnight index and not the term rate itself. Every other file
 * is returned unchanged.
 */
ORES_ORE_CORE_EXPORT input_files name_forwarding_curves(const input_files& files,
                                                        std::string_view market_data,
                                                        std::string_view portfolio);

}

#endif
