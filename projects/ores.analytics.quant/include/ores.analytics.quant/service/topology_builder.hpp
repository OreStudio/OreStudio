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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_TOPOLOGY_BUILDER_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_TOPOLOGY_BUILDER_HPP

#include "ores.analytics.quant/domain/ccy_pair_input.hpp"
#include "ores.analytics.quant/domain/crm_topology.hpp"
#include "ores.analytics.quant/export.hpp"
#include <string>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief Builds and validates the CRM's driver/derived spanning-tree
 * topology (phase 1 of the two-phase CRM interface).
 *
 * Uses Boost Graph: an incremental union-find (@c boost::disjoint_sets)
 * rejects any edge that would connect two already-connected currencies the
 * moment it is added -- so a config admitting more than one path between
 * two currencies is always caught here as a @c topology_error_kind::cycle_conflict
 * and never silently resolved. A final @c breadth_first_search from the
 * pivot assigns each currency its parent/edge in the tree.
 *
 * @c build() collects every violation across the whole input before
 * reporting; on any violation it throws a single
 * @c domain::topology_build_error carrying them all. Success returns an
 * immutable @c domain::crm_topology that never changes again.
 */
class ORES_ANALYTICS_QUANT_EXPORT topology_builder {
public:
    [[nodiscard]] static domain::crm_topology build(
        const std::vector<domain::ccy_pair_input>& pairs,
        const std::string& pivot_code,
        const std::vector<std::string>& required_majors);
};

} // namespace ores::analytics::quant::service

#endif
