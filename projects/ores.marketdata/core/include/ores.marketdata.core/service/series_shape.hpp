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
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.core/export.hpp"
#include <cstddef>
#include <map>
#include <optional>
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
 * @brief The value @p point holds on @p axis, or nothing when it holds none.
 */
ORES_MARKETDATA_CORE_EXPORT std::optional<std::string>
coordinate_of(const datum::market_datum& point, datum::field axis);

/**
 * @brief The grid a shape declares: every combination of the declared values of
 * its axes, in the order the shape stores the axes and with the last axis
 * varying fastest.
 *
 * A node is a position in the grid. Every read that returns a composite object
 * as nodes takes its nodes from here, so a single-instant read and an evolution
 * read cannot disagree about which node a point is or where it sits.
 */
class ORES_MARKETDATA_CORE_EXPORT series_grid final {
public:
    explicit series_grid(const series_shape& shape);

    /// The axes, in the order the shape stores them.
    const std::vector<datum::field>& axes() const {
        return shape_.axes;
    }

    /// The number of nodes, which is the product of the declared value counts.
    std::size_t size() const {
        return size_;
    }

    /// The label of each axis for node @p index, index for index with axes().
    std::vector<std::string> coordinates(std::size_t index) const;

    /**
     * @brief The node @p point belongs to, or nothing when it leaves an axis out
     * or holds a value the shape does not declare.
     */
    std::optional<std::size_t> node_of(const datum::market_datum& point) const;

private:
    series_shape shape_;
    std::vector<std::size_t> strides_;
    std::size_t size_ = 1;
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
