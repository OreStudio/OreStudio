/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.cli/config/add_currency_options.hpp"

#include <ostream>

namespace ores::cli::config {

std::ostream& operator<<(std::ostream& s, const add_currency_options& v) {
    s << "{ iso_code: " << v.iso_code
      << ", name: " << v.name
      << ", modified_by: " << v.modified_by;

    if (v.numeric_code) s << ", numeric_code: " << *v.numeric_code;
    if (v.symbol) s << ", symbol: " << *v.symbol;
    if (v.fraction_symbol) s << ", fraction_symbol: " << *v.fraction_symbol;
    if (v.fractions_per_unit) s << ", fractions_per_unit: " << *v.fractions_per_unit;
    if (v.rounding_type) s << ", rounding_type: " << *v.rounding_type;
    if (v.rounding_precision) s << ", rounding_precision: " << *v.rounding_precision;
    if (v.format) s << ", format: " << *v.format;
    if (v.asset_class) s << ", asset_class: " << *v.asset_class;
    if (v.market_tier) s << ", market_tier: " << *v.market_tier;

    s << " }";
    return s;
}

}
