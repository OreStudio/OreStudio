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

#include "ores.ore.api/domain/series_key_shape.hpp"
#include "ores.ore.core/export.hpp"
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

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
     * type. Storage fills that gap from series_key_registry::default_point_for()
     * rather than inventing a coordinate here, so the key still reconstructs
     * verbatim.
     *
     * Examples: "2Y", "25Y/10Y/ATM", "1Y/6M/0/0/0.025".
     */
    std::optional<std::string> point_id;
};

/**
 * @brief The key grammar of ORE market data, built from its shape table.
 *
 * Every ORE key follows the skeleton TYPE/METRIC/[QUALIFIER...]/[POINT_ID].
 * The split between qualifier and point is type-specific, and this value says
 * where it falls for each type: qualifier_depth counts the segments after the
 * metric that identify the series, and every remaining segment is the point.
 *
 * Build one from the rows of the shape table and pass it down. Nothing here
 * reads the database or reaches for a compiled table, so it is a plain value
 * that a test can build without a database, NATS or an import pipeline.
 */
class ORES_ORE_CORE_EXPORT series_key_registry final {
public:
    /**
     * @brief Builds the registry from the series key shape rows.
     *
     * @param shapes Rows of the series key shape table, as the repository
     *        returns them. A row whose type repeats replaces the earlier one.
     * @throws std::invalid_argument if shapes is empty, or if a row claims a
     *         point dimension and also carries a default point.
     */
    explicit series_key_registry(std::vector<domain::series_key_shape> shapes);

    /**
     * @brief Splits an ORE key into its structural parts.
     *
     * A type with no row is not an error. Its key's whole remainder becomes the
     * qualifier and the point stays null, so the key reconstructs verbatim and
     * a type ORE adds later never aborts an import.
     *
     * @param key Verbatim ORE key string (e.g. "DISCOUNT/RATE/EUR/CURVE/2Y").
     * @return Decomposed key components.
     * @throws std::invalid_argument if the key has fewer than two
     *         slash-separated segments (missing type or metric).
     */
    [[nodiscard]] decomposed_key decompose(const std::string& key) const;

    /**
     * @brief Reports whether a series type has a tenor or surface dimension.
     *
     * A type without one carries no point, so decompose() folds its whole
     * remainder into the qualifier and a consumer has nothing to read a point
     * from. This is a fact about the key's shape rather than about any
     * taxonomy, which is why it is answered here and not by a classifier.
     *
     * @return True if the type has a row and its keys carry a point; false for
     *         types the table does not know, which is the safe answer because
     *         their keys are folded into the qualifier too.
     */
    [[nodiscard]] bool has_point_dimension(const std::string& series_type) const;

    /**
     * @brief The point recorded for an observation whose key carries none.
     *
     * Every observation stores a point, so a series whose keys have no point of
     * their own needs one name for its single point. FX spot is the case that
     * matters: the point is a real tenor, SPOT, and that is what the table
     * holds. Types whose single point is not a tenor at all answer with an
     * empty string, which is the honest "no coordinate" value rather than an
     * invented tenor.
     *
     * @return The type's default point, or an empty string when it has none or
     *         the table does not know the type.
     */
    [[nodiscard]] std::string default_point_for(const std::string& series_type) const;

    /**
     * @brief Every series type the table carries, in ascending order.
     */
    [[nodiscard]] std::vector<std::string> known_series_types() const;

private:
    std::unordered_map<std::string, domain::series_key_shape> by_type_;
};

/**
 * @brief Reconstructs the ORE key string from its decomposed components.
 *
 * Inverse of series_key_registry::decompose; used by the serializer so that the
 * key field is never stored separately from the decomposed fields. It joins
 * strings and needs no table, so it stays a free function.
 */
ORES_ORE_CORE_EXPORT std::string reconstruct_key(const decomposed_key& dk);

}

#endif
