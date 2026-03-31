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
#ifndef ORES_ORE_MARKET_SERIES_KEY_REGISTRY_HPP
#define ORES_ORE_MARKET_SERIES_KEY_REGISTRY_HPP

#include <optional>
#include <string>

namespace ores::ore::market {

/**
 * @brief Result of decomposing an ORE market data key into its structural parts.
 *
 * Every ORE key follows the skeleton TYPE/METRIC/[QUALIFIER...]/[POINT_ID].
 * The split between qualifier and point_id is type-specific and defined by the
 * series_key_registry.  The full key is always reconstructable as:
 *
 *   series_type + "/" + metric + "/" + qualifier
 *       + (point_id ? "/" + *point_id : "")
 */
struct decomposed_key {
    /**
     * @brief ORE key type (e.g. DISCOUNT, MM, FX, SWAPTION).
     */
    std::string series_type;

    /**
     * @brief Metric component (e.g. RATE, PRICE, RATE_LNVOL, BASIS_SPREAD).
     */
    std::string metric;

    /**
     * @brief Type-specific qualifier grouping all points of the series.
     *
     * Examples: "EUR" (DISCOUNT), "EUR/CHF" (FX), "CHF" (SWAPTION),
     *           "USD/3M/CHF/3M" (CC_BASIS_SWAP).
     * For unknown types the entire remainder after metric becomes the qualifier.
     */
    std::string qualifier;

    /**
     * @brief Point identifier within the series (tenor, surface coordinate).
     *
     * Null for scalar series (e.g. FX spot, equity spot, recovery rate) and
     * for unknown types (safe fallback — qualifier absorbs all remaining
     * segments).
     *
     * Examples: "2Y", "25Y/10Y/ATM", "1Y/6M/0/0/0.025".
     */
    std::optional<std::string> point_id;
};

/**
 * @brief Decomposes an ORE market data key into its structural parts.
 *
 * Uses a static per-type registry to determine how many segments after the
 * metric belong to the qualifier.  For types not present in the registry the
 * entire remainder (after type/metric) becomes the qualifier and point_id is
 * left null, guaranteeing a lossless roundtrip for all keys.
 *
 * @param key Verbatim ORE key string (e.g. "DISCOUNT/RATE/EUR/CURVE/2Y").
 * @return Decomposed key components.
 * @throws std::invalid_argument if the key has fewer than two slash-separated
 *         segments (missing type or metric).
 */
decomposed_key decompose_key(const std::string& key);

/**
 * @brief Reconstructs the ORE key string from its decomposed components.
 *
 * Inverse of decompose_key; used by the serializer so that the key field
 * is never stored separately from the decomposed fields.
 */
std::string reconstruct_key(const decomposed_key& dk);

}

#endif
