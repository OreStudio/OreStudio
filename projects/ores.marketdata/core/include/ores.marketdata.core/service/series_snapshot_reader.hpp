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
#ifndef ORES_MARKETDATA_CORE_SERVICE_SERIES_SNAPSHOT_READER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_SERIES_SNAPSHOT_READER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include <chrono>

namespace ores::marketdata::service {

/**
 * @brief Reads a composite object as it stands at one instant, as values against
 * the grid the shape declares.
 *
 * The grid and the placement of each point are the evolution read's: both take
 * them from series_grid, so a snapshot and an evolution agree on the nodes and
 * on the node a point sits at. A snapshot carries the latest value of a node
 * forward from before the instant, and an evolution states only what an instant
 * states. The snapshot adds the record time of each value and the staleness of
 * the whole.
 */
class ORES_MARKETDATA_CORE_EXPORT series_snapshot_reader final {
public:
    /**
     * @brief Reads the object @p request names, as of @p as_of.
     *
     * @param ctx Database context, already tenant-scoped by the caller.
     * @param request The object.
     * @param as_of The instant the snapshot is read at, which the ages are
     * measured from.
     * @return The snapshot, or @c success false with the reason. An object with
     * no declared axis and an identity that names several series are each
     * refused with the reason. An identity that names no series is an empty
     * snapshot.
     * @throws std::runtime_error if a stored oresmd URI does not read.
     */
    static messaging::get_curve_snapshot_response
    read(ores::database::context ctx,
         const messaging::get_curve_snapshot_request& request,
         std::chrono::system_clock::time_point as_of);
};

}

#endif
