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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MARKETDATA_API_PROJECTION_ORE_PROJECTION_HPP
#define ORES_MARKETDATA_API_PROJECTION_ORE_PROJECTION_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.marketdata.api/export.hpp"
#include <expected>
#include <string>
#include <string_view>
#include <unordered_map>

namespace ores::marketdata::projection {

/**
 * @brief The vendor projection to ORE: an oresmd identifier written as ORE
 * names it.
 *
 * The page Market Data Identifier defines it as one-directional, an
 * identifier rendered in a vendor's notation; reading an ORE name back is
 * ingestion, which the importer owns. An index is written by its ORE index
 * name and a quote by its ORE market data key, both fixed by the codecs. A
 * curve has no fixed ORE name: ORE finds it through the market configuration
 * a run reads, so the run gives the projection the name its configuration
 * gives each curve series. A projection built with no names writes no curve.
 */
class ORES_MARKETDATA_API_EXPORT ore_projection final {
public:
    ore_projection() = default;

    /// @param curve_names The ORE name of each curve series, by its URI.
    explicit ore_projection(std::unordered_map<std::string, std::string> curve_names);

    /// The ORE name of @p reference, or why it has none here.
    [[nodiscard]] std::expected<std::string, std::string>
    ore_name_of(const datum::market_reference& reference) const;

    /**
     * @brief The ORE name of what a stored column holds, or the text as stored.
     *
     * Until every stored reference is an identifier, a column may hold an ORE
     * name already, and a curve the run names no ORE name for is written as
     * the trade states it, so an export with no names writes what its trades
     * hold.
     */
    [[nodiscard]] std::string ore_name_or_stored(std::string_view stored) const;

private:
    std::unordered_map<std::string, std::string> curve_names_;
};

}

#endif
