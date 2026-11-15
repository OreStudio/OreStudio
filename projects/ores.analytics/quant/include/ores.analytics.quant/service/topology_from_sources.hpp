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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_TOPOLOGY_FROM_SOURCES_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_TOPOLOGY_FROM_SOURCES_HPP

#include "ores.analytics.quant/domain/ccy_pair_input.hpp"
#include "ores.analytics.quant/export.hpp"
#include <expected>
#include <string>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief One publisher's fixings, as the store records their identity: the base
 * currency every series of that publisher is quoted against, the currencies it
 * quotes, and the rank resolution reads it at.
 *
 * A fixing series states the currency it prices and the currency it prices it
 * in. A publisher quotes all of them against one base, so the base is the same
 * for every series of that source and is what the pair composition cancels.
 */
struct fixing_source {
    std::string provider_code;
    /// The currency every series of this source is quoted against.
    std::string base_code;
    /// Where the provider sits when resolution ranks it; lower is read first.
    int precedence = 1000;
    /// The currencies the source quotes, as the identity of its series states them.
    std::vector<std::string> currencies;
};

/// The topology one source describes: a star rooted at the base the source quotes
/// everything against, with one edge per currency it quotes.
struct source_topology {
    std::string provider_code;
    std::string pivot_code;
    std::vector<domain::ccy_pair_input> pairs;
};

/**
 * @brief Chooses the source a party's rates are read from and states the driver
 * pairs its topology is built from.
 *
 * The choice is the rank the store carries: the source with the lowest
 * precedence that can anchor a tree wins, and a source quoting fewer than two
 * currencies cannot anchor one. The pairs are the star of that source against
 * its own base, which becomes the pivot, so the base cancels in every derived
 * pair and no two publishers are ever mixed in one rate.
 *
 * Nothing here names a currency or a publisher: the caller supplies both from
 * data. The spanning tree, its validation and the choice of a path to the pivot
 * are @c topology_builder's business, not this function's.
 *
 * @return The chosen source and its pairs, or why no source could anchor a tree.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::expected<source_topology, std::string>
topology_from_sources(const std::vector<fixing_source>& sources);

}

#endif
