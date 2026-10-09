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
#ifndef ORES_MARKETDATA_CORE_SERVICE_SERIES_SHAPE_WRITER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_SERIES_SHAPE_WRITER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Declares a composite series' shape: its axes and the ordered values of
 * each, taken from the points a source states.
 *
 * The axes are the coordinate fields the series' instrument type declares, in
 * the order its schema row writes them. The values of an axis are the distinct
 * texts its points hold for that field, in the order the points present them. A
 * series whose type declares no coordinate has no shape, and declare() writes
 * nothing for it.
 *
 * A coordinate the type makes optional is left out when no point carries it, so
 * an axis nothing states constrains nothing. A required coordinate that no point
 * carries is refused.
 *
 * A second source for the same series appends to the vocabulary the first
 * declared. It writes only the values the series does not already hold, and each
 * new value takes the place after the values already there, so the stored order
 * survives.
 *
 * An axis value that is a term may be checked against the reference tenor
 * table. The check runs before any row is written, so a refused shape leaves no
 * rows behind.
 *
 * This lives in ores.marketdata.core rather than in ores.marketdata.service,
 * because the import declares the shape of the series it carries and the import
 * is a core service. The reference tenor read is what ties it to
 * ores.refdata.core, so the core library links that too.
 */
class ORES_MARKETDATA_CORE_EXPORT series_shape_writer final {
public:
    /**
     * @brief Whether the term values of a declared shape are checked against the
     * reference tenor table.
     *
     * A build draws its pillars from the tenant's own curve configuration, so a
     * pillar the reference table does not hold is a mistake and the shape is
     * refused. An import records the terms the file states, and those are the
     * vendor's rather than the tenant's: the ORE example corpus writes tenors
     * such as 13Y and 1Y6M that no reference table holds, so checking an
     * imported shape would refuse valid market data.
     */
    enum class term_check : std::uint8_t {
        /// Refuse a term value the reference tenor table does not hold.
        against_reference_tenors,
        /// Record the term values as the source states them.
        as_stated
    };

    /**
     * @brief Writes @p series' axes and their values from @p points.
     *
     * The series id and the party id are explicit because the series datum
     * carries no id: a datum names its series by its identity fields, and the
     * caller already holds the row's own key.
     *
     * @param ctx Database context, already tenant-scoped by the caller. The
     * writes join its transaction.
     * @param series The series whose shape this is.
     * @param series_id The primary key of the series row.
     * @param party_id The party that owns the series.
     * @param points One datum per point the source states.
     * @param check Whether @p points' term values are checked against the
     * reference tenor table.
     * @throws std::invalid_argument if a required axis has no value in
     * @p points, or @p check is against_reference_tenors and a term value is not
     * a code the reference tenor table holds.
     */
    static void declare(ores::database::context ctx,
                        const datum::market_datum& series,
                        const boost::uuids::uuid& series_id,
                        const boost::uuids::uuid& party_id,
                        const std::vector<datum::market_datum>& points,
                        term_check check);
};

}

#endif
