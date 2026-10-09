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
#ifndef ORES_MARKETDATA_CORE_SERVICE_SERIES_SLICE_READER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_SERIES_SLICE_READER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"

namespace ores::marketdata::service {

/**
 * @brief Reads one component of a composite object as a term structure at each
 * instant in a range.
 *
 * The object is named the way the resolver names one, by typed identity fields,
 * and the component by an axis and a value the object declares. The answer holds
 * the declared ladder once, in the shape's order, and one row per instant whose
 * values are index for index with it, so two instants are drawn against one
 * ladder and a node with no value is a hole rather than a shorter curve.
 *
 * The ladder is the shape's and not the points': an axis the object declares and
 * no point fills is still a node. The instants are the ones the object holds in
 * the range, and each carries the object as it stood at that instant, so a point
 * that did not move keeps the value it last held.
 *
 * An object with one coordinate axis has one component, the whole object, and
 * the request names no component. An object with two axes is a component once
 * one axis is fixed. An object with more than two axes is a surface of surfaces,
 * so its slice is not a term structure and the read refuses it.
 */
class ORES_MARKETDATA_CORE_EXPORT series_slice_reader final {
public:
    /**
     * @brief The most instants one read returns.
     *
     * A range is the caller's, so a long one would return an unbounded matrix.
     * The ceiling is enforced server-side, like the bucket count, because a
     * caller that asked for a quarter and silently received a day would read the
     * answer as complete. A refused read says how many instants the range holds.
     */
    static constexpr unsigned int max_instants = 200;

    /**
     * @brief Reads the component @p request names.
     *
     * @param ctx Database context, already tenant-scoped by the caller.
     * @param request The object, the component and the range.
     * @return The component at each instant, or @c success false with the
     * reason. A component the shape does not declare is refused, naming the axis
     * or the value that is missing. Nothing else throws for a bad request.
     * @throws std::runtime_error if the range holds more instants than
     * @c max_instants, or a stored oresmd URI does not read.
     */
    static messaging::get_series_slice_response
    read(ores::database::context ctx, const messaging::get_series_slice_request& request);
};

}

#endif
