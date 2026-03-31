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
#ifndef ORES_ORE_MARKET_FIXING_HPP
#define ORES_ORE_MARKET_FIXING_HPP

#include <chrono>
#include <string>

namespace ores::ore::market {

/**
 * @brief A single entry from an ORE fixings file (fixings*.txt).
 *
 * Faithfully represents one line of the whitespace-delimited three-column
 * format: DATE INDEX_NAME VALUE.  Structurally identical to market_datum
 * but semantically distinct — the second field is an index name rather than
 * an instrument quote key.
 */
struct fixing {
    /**
     * @brief The fixing date.
     */
    std::chrono::year_month_day date;

    /**
     * @brief The index name stored verbatim from the file (e.g. "EUR-EONIA").
     *
     * Retained for legacy consumers and debugging.
     */
    std::string index_name;

    /**
     * @brief The fixing value, stored as a raw string.
     *
     * Preserving the original string ensures exact round-trip fidelity.
     */
    std::string value;

    // ── Decomposed fields ─────────────────────────────────────────────────────

    /**
     * @brief Qualifier for the series catalog — equals index_name.
     *
     * Fixings do not follow the TYPE/METRIC/QUALIFIER/POINT_ID key structure;
     * the index_name is the full series identifier.  The qualifier field
     * mirrors it so that the fixing can be joined to ores_marketdata_series_tbl
     * without a separate mapping step.
     */
    std::string qualifier;
};

} // namespace ores::ore::market

#endif
