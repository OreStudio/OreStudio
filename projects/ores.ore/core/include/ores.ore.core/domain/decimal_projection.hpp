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
#ifndef ORES_ORE_CORE_DOMAIN_DECIMAL_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_DECIMAL_PROJECTION_HPP

#include "ores.ore.core/xml/record.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <optional>
#include <string_view>

namespace ores::ore::domain {

/**
 * @brief An exact decimal member as the double the trading domain stores.
 *
 * The table keeps the digits the document wrote, and a continuous domain
 * column is a double. The conversion happens here, once, at the boundary.
 */
inline auto decimal_as_double(const xml::record_reader& r, std::string_view name)
    -> std::optional<double> {
    if (const auto value = r.decimal(name))
        return value->to_double();
    return std::nullopt;
}

/// A double as the exact decimal the table writes.
inline auto decimal_from_double(double value) -> ores::utility::decimal::decimal {
    if (auto parsed = ores::utility::decimal::decimal::from_double(value))
        return *parsed;
    return ores::utility::decimal::decimal{};
}

/// States a decimal member from a double the domain holds.
inline void set_decimal_from_double(xml::record_writer& w, std::string_view name, double value) {
    w.set_decimal(name, decimal_from_double(value));
}

}

#endif
