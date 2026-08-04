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
#ifndef ORES_SYNTHETIC_API_DOMAIN_SCOPE_HPP
#define ORES_SYNTHETIC_API_DOMAIN_SCOPE_HPP

namespace ores::synthetic::domain {

/**
 * @brief Sharing radius of a market_data_generation_config: who consumes
 * the same generated data.
 *
 * Orthogonal to binding_mode, which decides whether the generated data
 * is authoritative. party_id/tenant_id are null/set per level: system
 * (both null), tenant (tenant_id only), party (both set).
 */
enum class scope {
    system, ///< Shared across every tenant. Not exercised end-to-end yet.
    tenant, ///< Shared across every party under one tenant.
    party   ///< Owned by, and visible only to, a single party.
};

}

#endif
