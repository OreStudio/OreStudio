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
#ifndef ORES_MARKETDATA_CORE_SERVICE_SERIES_SHAPE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_SERIES_SHAPE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.core/export.hpp"
#include <map>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief The declared shape of one composite series: its coordinate axes and
 * the values each axis declares.
 *
 * Both are in the order the shape tables store them, which is the order the
 * schema row writes its coordinates and the order the first source stated the
 * values. A read that draws an axis takes this and nothing else, so the slice
 * read and the evolution read cannot disagree about what an axis is.
 */
struct ORES_MARKETDATA_CORE_EXPORT series_shape final {
    /// The coordinate axes, in the order the schema row writes them.
    std::vector<datum::field> axes;

    /// The values each axis declares, in the order the shape stores them.
    std::map<datum::field, std::vector<std::string>> values;
};

/**
 * @brief Reads the declared shape of @p series_id from the shape tables.
 *
 * @param ctx Database context, already tenant-scoped by the caller.
 * @param series_id The series, as the text of its key.
 * @throws std::runtime_error if a shape row names a field the schema does not.
 */
ORES_MARKETDATA_CORE_EXPORT series_shape read_series_shape(ores::database::context ctx,
                                                           const std::string& series_id);

}

#endif
