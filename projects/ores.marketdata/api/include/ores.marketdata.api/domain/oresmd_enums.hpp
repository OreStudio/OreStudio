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
#ifndef ORES_MARKETDATA_API_DOMAIN_ORESMD_ENUMS_HPP
#define ORES_MARKETDATA_API_DOMAIN_ORESMD_ENUMS_HPP

namespace ores::marketdata::domain {

/**
 * @brief The `type` query key of an oresmd URI: what kind of thing the identifier names,
 * independent of its coordinates.
 *
 * See id:C3E053CA-0D4B-480B-9119-E11530160EC1 ("oresmd: ORE Studio Market Data URI"),
 * "Grammar" section.
 */
enum class instrument_type {
    fixing, ///< A rate fixing/index (projects to ORE's index name).
    curve,  ///< A whole curve (projects to ORE's Yield/<CCY>/<CURVE_ID> curve key).
    quote,  ///< A single published quote (projects to an ORE quote key).
    vol     ///< A volatility surface point (projects to an ORE quote key).
};

/**
 * @brief The `role` query key of an oresmd URI, IR-only: whether a curve discounts or
 * projects, closing the gap-analysis's discount-vs-projection ambiguity.
 */
enum class curve_role {
    discount,        ///< This curve discounts cashflows.
    projection,      ///< This curve projects a floating index's forward rate.
    self_discounting ///< Degenerate case: one curve does both (current synthetic data's shape).
};

/**
 * @brief The `metric` query key of an oresmd URI, only meaningful when `type=quote`:
 * disambiguates a par-rate quote from a curve-sampled discount factor at the same
 * tenor/point coordinate.
 */
enum class metric {
    par_rate,       ///< A par-rate quote (e.g. ORE's IR_SWAP/RATE quote type).
    discount_factor ///< A directly-quoted discount factor (e.g. ORE's DISCOUNT/RATE quote type).
};

/**
 * @brief The `index` query key of an oresmd URI, IR-only: a fixed benchmark-family token,
 * not free text -- closes the gap-analysis's "index_name is free text" finding.
 */
enum class index_family { libor, euribor, sofr, estr, sonia, tona };

}

#endif
