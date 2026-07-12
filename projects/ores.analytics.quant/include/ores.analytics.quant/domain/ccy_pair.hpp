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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_CCY_PAIR_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_CCY_PAIR_HPP

#include "ores.analytics.quant/domain/currency_id.hpp"

namespace ores::analytics::quant::domain {

/// A quoted currency pair within an already-built topology: base/quote as
/// resolved @c currency_id handles, not raw ISO codes. @c is_driver carries
/// forward the driver/derived assignment the edge was built from -- see
/// @c ccy_pair_input. Note @c rate_engine::update does not currently read
/// this field: it derives accumulation direction purely from which side
/// of an incoming tick matches the tree's parent/child structure, so
/// either side of an edge may be ticked regardless of which was marked
/// the driver at build time.
struct ccy_pair {
    currency_id base;
    currency_id quote;
    bool is_driver;

    friend constexpr bool operator==(const ccy_pair& lhs, const ccy_pair& rhs) noexcept {
        return lhs.base == rhs.base && lhs.quote == rhs.quote && lhs.is_driver == rhs.is_driver;
    }
    friend constexpr bool operator!=(const ccy_pair& lhs, const ccy_pair& rhs) noexcept {
        return !(lhs == rhs);
    }
};

} // namespace ores::analytics::quant::domain

#endif
