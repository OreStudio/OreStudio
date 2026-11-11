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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MARKETDATA_SERVICE_APP_FX_VOL_SURFACE_GRID_HPP
#define ORES_MARKETDATA_SERVICE_APP_FX_VOL_SURFACE_GRID_HPP

#include "ores.marketdata.service/export.hpp"
#include <expected>
#include <string>
#include <vector>

namespace ores::marketdata::service::app {

/// One point of a surface as the store holds it.
struct surface_grid_point final {
    std::string expiry;
    std::string strike;
    double volatility = 0.0;
};

/// A surface, or a slice of it, with its axes in order.
struct surface_grid final {
    std::vector<std::string> expiries;
    std::vector<std::string> strikes;
    /// Expiry by expiry, each expiry's strikes in order.
    std::vector<surface_grid_point> points;
};

/**
 * @brief Orders a surface's points and cuts the slice asked for.
 *
 * Expiries are ordered by their length, shortest first, and strikes by @p strike_order, the
 * blueprint's own order. A non-empty @p expiry keeps that expiry across the strikes, and a
 * non-empty @p strike keeps that strike across the expiries. Refuses a slice the surface does not
 * hold, and an expiry that is not a tenor.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::expected<surface_grid, std::string>
surface_grid_of(std::vector<surface_grid_point> points,
                const std::vector<std::string>& strike_order,
                const std::string& expiry,
                const std::string& strike);

}

#endif
