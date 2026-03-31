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
#ifndef ORES_ORE_MARKET_MARKET_DATUM_HPP
#define ORES_ORE_MARKET_MARKET_DATUM_HPP

#include <chrono>
#include <optional>
#include <string>

namespace ores::ore::market {

/**
 * @brief A single entry from an ORE market data quote file (market*.txt).
 *
 * Faithfully represents one line of the whitespace-delimited three-column
 * format: DATE KEY VALUE.  The key is stored verbatim for reference and is
 * also decomposed into (series_type, metric, qualifier, point_id) by the
 * series_key_registry at parse time.  The full key is always reconstructable
 * as: series_type + "/" + metric + "/" + qualifier [+ "/" + point_id].
 */
struct market_datum {
    /**
     * @brief The observation date.
     */
    std::chrono::year_month_day date;

    /**
     * @brief The instrument quote key stored verbatim from the file.
     *
     * Retained for debugging and legacy consumers.  The serializer emits the
     * key reconstructed from the decomposed fields to validate roundtrip
     * correctness.
     */
    std::string key;

    /**
     * @brief The quote value, stored as a raw string.
     *
     * Preserving the original string ensures exact round-trip fidelity
     * (e.g. "0.0074600000" is retained as-is, without floating-point loss).
     */
    std::string value;

    // ── Decomposed key fields (populated by the parser via series_key_registry) ──

    /**
     * @brief ORE key type component (e.g. DISCOUNT, MM, FX, SWAPTION).
     */
    std::string series_type;

    /**
     * @brief Metric component (e.g. RATE, PRICE, RATE_LNVOL, BASIS_SPREAD).
     */
    std::string metric;

    /**
     * @brief Type-specific qualifier grouping all points of the series.
     *
     * Examples: "EUR" (DISCOUNT curve), "EUR/CHF" (FX spot or vol),
     *           "CHF" (swaption vol surface), "USD/3M/CHF/3M" (CC basis).
     */
    std::string qualifier;

    /**
     * @brief Tenor or surface coordinate within the series.
     *
     * Null for scalar series (FX spot, equity spot, recovery rate) and for
     * types not present in the registry (safe fallback).
     *
     * Examples: "2Y", "25Y/10Y/ATM", "1Y/6M/0/0/0.025".
     */
    std::optional<std::string> point_id;
};

} // namespace ores::ore::market

#endif
