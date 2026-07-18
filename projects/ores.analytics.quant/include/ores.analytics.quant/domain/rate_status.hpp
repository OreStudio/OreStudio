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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_RATE_STATUS_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_RATE_STATUS_HPP

namespace ores::analytics::quant::domain {

enum class rate_status {
    /// Every driver on the path is within the staleness policy's stale_after age.
    fresh,
    /// At least one contributing driver is older than stale_after but within
    /// disconnected_after.
    stale,
    /// At least one contributing driver is older than disconnected_after —
    /// its feed has effectively stopped, not just lagged.
    disconnected,
    /// At least one driver on the path has never received a tick.
    unavailable,
};

} // namespace ores::analytics::quant::domain

#endif
