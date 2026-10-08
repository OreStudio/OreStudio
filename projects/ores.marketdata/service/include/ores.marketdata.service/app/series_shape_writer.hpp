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
#ifndef ORES_MARKETDATA_SERVICE_APP_SERIES_SHAPE_WRITER_HPP
#define ORES_MARKETDATA_SERVICE_APP_SERIES_SHAPE_WRITER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.service/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief Declares a composite series' shape: its axes and the ordered values of
 * each, taken from the points a build produced.
 *
 * The axes are the coordinate fields the series' instrument type declares, in
 * the order its schema row writes them. The values of an axis are the distinct
 * texts its points hold for that field, in the order the points present them. A
 * series whose type declares no coordinate has no shape, and declare() writes
 * nothing for it.
 *
 * An axis value that is a term must be a code the reference tenor table holds.
 * The check runs before any row is written, so a refused shape leaves no rows
 * behind.
 */
class ORES_MARKETDATA_SERVICE_EXPORT series_shape_writer final {
public:
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
     * @param points One datum per point the build produced.
     * @throws std::invalid_argument if an axis has no value in @p points, or a
     * term value is not a code the reference tenor table holds.
     */
    static void declare(ores::database::context ctx,
                        const datum::market_datum& series,
                        const boost::uuids::uuid& series_id,
                        const boost::uuids::uuid& party_id,
                        const std::vector<datum::market_datum>& points);
};

}

#endif
