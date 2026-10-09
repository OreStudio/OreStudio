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
#ifndef ORES_MARKETDATA_CORE_SERVICE_SERIES_EVOLUTION_READER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_SERIES_EVOLUTION_READER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"

namespace ores::marketdata::service {

/**
 * @brief Reads a whole composite object as instants by nodes, with the change
 * each node shows against the shape.
 *
 * The grid is the shape's: every combination of the declared values of the
 * object's axes, in the order the shape stores the axes and with the last axis
 * varying fastest. Every instant's values are index for index with it, so the
 * answer is a matrix and not an array of objects, and a node the shape declares
 * and no instant carries is a cell all the same. That is what tells it apart
 * from a node the object does not have, which is not in the answer at all.
 *
 * Each node carries one word for the change it shows over the range: persistent
 * when every instant carries it, added when the last does and the first does
 * not, dropped when the first does and the last does not, intermittent when
 * some other instant does, and never_quoted when none does.
 *
 * The instants are the ones the object holds between two instants, or the set
 * the caller states, which replaces the range.
 */
class ORES_MARKETDATA_CORE_EXPORT series_evolution_reader final {
public:
    /**
     * @brief The most instants one read returns.
     *
     * The ceiling is the slice read's, for the same reason: a range is the
     * caller's and a long one would return an unbounded matrix. A refused read
     * says how many instants the range holds.
     */
    static constexpr unsigned int max_instants = 200;

    /**
     * @brief Reads the object @p request names.
     *
     * @param ctx Database context, already tenant-scoped by the caller.
     * @param request The object and the range or the set of instants.
     * @return The matrix, or @c success false with the reason. An object with
     * no declared axis, an identity that names several series, and a range that
     * ends before it starts are each refused with the reason.
     * @throws std::runtime_error if more instants are read than
     * @c max_instants, or a stored oresmd URI does not read.
     */
    static messaging::get_series_evolution_response
    read(ores::database::context ctx, const messaging::get_series_evolution_request& request);
};

}

#endif
