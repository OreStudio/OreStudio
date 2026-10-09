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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MARKET_SERIES_IDENTITY_PROJECTOR_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MARKET_SERIES_IDENTITY_PROJECTOR_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.core/export.hpp"
#include <cstddef>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief Writes the joinable identity of each series, read from its oresmd URI
 * through the codec.
 *
 * The series row carries its identity only inside its URI, so a query cannot
 * join on it. This projects that identity into one column per field the codec
 * schema declares, and writes the row in the same transaction as the series
 * when the context carries one.
 *
 * The projection is a projection: it is written from the URI and never read
 * back to rebuild one, and the codec stays the only thing that reads the
 * grammar. A URI neither =oresmd_uri_codec::read= nor =read_index= admits is
 * recorded with its kind and no field value rather than refused, so a series
 * that already holds such a URI stays findable and a write cannot fail because
 * of data an earlier writer left behind.
 */
class ORES_MARKETDATA_CORE_EXPORT market_series_identity_projector final {
public:
    /**
     * @brief What a re-projection wrote, left alone and could not read.
     *
     * A projection an earlier projector wrote carries a fixing's kind and its
     * asset class and no field value. Re-projecting compares a fresh projection
     * with the stored row and writes only the rows that differ, so a call is
     * idempotent and repairs the rows an earlier projector wrote.
     */
    struct reprojection_result {
        std::size_t written = 0;
        std::size_t unchanged = 0;
        std::size_t unreadable = 0;
    };

    /**
     * @brief Projects @p series, replacing the identity of any series already
     * projected.
     *
     * @return How many of @p series carried a URI neither codec reads. Their
     * rows are still written, with their kind and no field value, so the count
     * says how many identities the codec could not state rather than how many
     * were dropped -- a write path ignores it, and the backfill reports it.
     */
    static std::size_t project(ores::database::context ctx,
                               const std::vector<domain::market_series>& series);

    /**
     * @brief Projects @p series and writes only the rows whose projection
     * changed.
     *
     * The write path projects a series as it writes it, so every row it has
     * seen is already current. A row an earlier projector wrote, or one whose
     * URI changed, is not: this compares a fresh projection with what the table
     * holds and writes the difference. A second call over the same series finds
     * nothing to write, so the backfill repairs the rows the current projector
     * would spell differently however often it runs.
     */
    static reprojection_result reproject(ores::database::context ctx,
                                         const std::vector<domain::market_series>& series);
};

}

#endif
