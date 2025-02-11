/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <iomanip>
#include <ostream>
#include "ores.core/types/currency.hpp"

namespace ores::core::types {

std::ostream& operator<<(std::ostream& os, const currency& v) {
    using std::quoted;
    os << " { "
       << quoted("__type__") << ": " << quoted("ores::core::ore::model::currency") << ", "
       << quoted("name") << ": " << quoted(v.name()) << ", "
       << quoted("iso_code") << ": " << quoted(v.iso_code()) << ", "
       << quoted("numeric_code") << ": " << v.numeric_code() << ", "
       << quoted("symbol") << ": " << quoted(v.symbol()) << ", "
       << quoted("fraction_symbol") << ": " << quoted(v.fraction_symbol()) << ", "
       << quoted("fractions_per_unit") << ": " << v.fractions_per_unit() << ", "
       << quoted("rounding_type") << ": " << quoted(v.rounding_type()) << ", "
       << quoted("rounding_precision") << ": " << v.rounding_precision() << ", "
       << quoted("format") << ": " << quoted(v.format())
       << " }";
    return os;
}

}
