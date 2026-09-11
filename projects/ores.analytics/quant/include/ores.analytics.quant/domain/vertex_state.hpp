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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_VERTEX_STATE_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_VERTEX_STATE_HPP

#include <chrono>

namespace ores::analytics::quant::domain {

/**
 * @brief One currency's state within a @c rate_snapshot: immutable once
 * constructed, replaced wholesale (not mutated) on every @c rate_engine
 * update.
 *
 * Carries both the vertex's own edge-to-parent (so an unrelated update
 * elsewhere in the tree can recompute this vertex's cumulative values
 * without re-reading the original tick) and the cumulative, pivot-relative
 * values a read actually wants:
 *
 * - @c log_rate = sum of every edge's log-rate delta from the pivot down
 *   to this vertex -- so any derived rate is one subtraction away.
 * - @c as_of = the oldest contributing tick's timestamp anywhere on that
 *   path -- a chain is only as fresh as its stalest link.
 * - @c valid = whether every edge on that path has received at least one
 *   tick yet.
 */
struct vertex_state {
    /// This vertex's own edge-to-parent, log-space (0 for the pivot).
    double edge_log_delta = 0.0;
    std::chrono::system_clock::time_point edge_observed_at{};
    bool edge_valid = false;

    /// Cumulative from the pivot down to this vertex.
    double log_rate = 0.0;
    std::chrono::system_clock::time_point as_of{};
    bool valid = false;
};

} // namespace ores::analytics::quant::domain

#endif
