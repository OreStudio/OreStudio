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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_SERIES_SHAPE_CHECK_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_SERIES_SHAPE_CHECK_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.core/export.hpp"
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief Refuses an observation whose point is not a node of its series'
 * declared shape.
 *
 * A composite series states the axes it varies over and the values of each in
 * the series axis tables. An observation writes one node of that shape, so a
 * point the shape does not name is a defect in the writer rather than market
 * data. This check reads the shape of the series the batch names, once for the
 * batch, and refuses the first observation that carries a value no axis of its
 * series declares.
 *
 * A series with no axis rows declares no shape, so every observation of it is
 * accepted: a series that predates the shape tables keeps taking points. A URI
 * the codec cannot read is accepted and logged: the identity projection already
 * records such a URI as a defect, and it is not this check's to judge.
 *
 * Nothing is written here, so a refused batch leaves the store untouched.
 */
class ORES_MARKETDATA_CORE_EXPORT series_shape_check final {
public:
    /**
     * @brief Refuses the first observation whose point leaves the shape its
     * series declares.
     *
     * @throws std::invalid_argument naming the series id, the axis field and
     * the value the point carries, in that order, when the value is not one
     * the axis declares.
     */
    static void check(ores::database::context ctx,
                      const std::vector<domain::market_observation>& observations);
};

}

#endif
