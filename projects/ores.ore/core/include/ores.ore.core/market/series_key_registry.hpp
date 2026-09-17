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
#ifndef ORES_ORE_CORE_MARKET_SERIES_KEY_REGISTRY_HPP
#define ORES_ORE_CORE_MARKET_SERIES_KEY_REGISTRY_HPP

#include "ores.ore.core/export.hpp"
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
     * Null when the key carries no point of its own: a type without a point
     * dimension, a key shorter than its type's qualifier depth, or an unknown
     * type. Storage fills that gap from default_point_for() rather than
     * inventing a coordinate here, so the key still reconstructs verbatim.
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
ORES_ORE_CORE_EXPORT decomposed_key decompose_key(const std::string& key);

/**
 * @brief Reconstructs the ORE key string from its decomposed components.
 *
 * Inverse of decompose_key; used by the serializer so that the key field
 * is never stored separately from the decomposed fields.
 */
ORES_ORE_CORE_EXPORT std::string reconstruct_key(const decomposed_key& dk);

/**
 * @brief Reports whether an ORE series type has a tenor or surface dimension.
 *
 * A type without one carries no point_id, so decompose_key folds its whole
 * remainder into the qualifier and a consumer has nothing to read a point
 * from. This is a fact about the key's shape rather than about any taxonomy,
 * which is why it is answered here and not by a classifier.
 *
 * @param series_type ORE key type.
 * @return True if the type is registered and its keys carry a point; false
 *         for types the registry does not know, which is the safe answer
 *         because their keys are folded into the qualifier too.
 */
ORES_ORE_CORE_EXPORT bool has_point_dimension(const std::string& series_type);

/**
 * @brief The point recorded for an observation whose key carries none.
 *
 * Every observation stores a point, so a series whose keys have no point of
 * their own needs one name for its single point. FX spot is the case that
 * matters: the point is a real tenor, SPOT, and that is what the table holds.
 * Types whose single point is not a tenor at all answer with an empty string,
 * which is the honest "no coordinate" value rather than an invented tenor.
 *
 * @param series_type ORE key type.
 * @return The type's default point, or an empty string when it has none or
 *         the registry does not know the type.
 */
ORES_ORE_CORE_EXPORT std::string default_point_for(const std::string& series_type);

}

#endif
