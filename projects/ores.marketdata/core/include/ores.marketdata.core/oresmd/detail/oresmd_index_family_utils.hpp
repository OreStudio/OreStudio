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
#ifndef ORES_MARKETDATA_CORE_ORESMD_DETAIL_ORESMD_INDEX_FAMILY_UTILS_HPP
#define ORES_MARKETDATA_CORE_ORESMD_DETAIL_ORESMD_INDEX_FAMILY_UTILS_HPP

#include "ores.marketdata.api/domain/oresmd_enums.hpp"

namespace ores::marketdata::core::detail {

/**
 * @brief True for overnight benchmarks (no tenor suffix in ORE's index-name convention,
 * e.g. "USD-SOFR"), false for term benchmarks that require one (e.g. "USD-LIBOR-3M").
 * Shared by oresmd_parser (tenor-required validation) and oresmd_projections (index-name
 * projection), so the two classifications can't drift apart.
 *
 * Mirrors the tenor rule of the CHECK constraint on
 * ores_synthetic_ir_curve_generation_configs_tbl
 * (synthetic_ir_curve_generation_configs_create.sql): libor and euribor are the only term families
 * (tenor <> ''); all 20 others are overnight-style (tenor = '').
 */
inline bool is_overnight(domain::index_family f) {
    return f != domain::index_family::libor && f != domain::index_family::euribor;
}

}

#endif
