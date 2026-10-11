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
#ifndef ORES_ORE_CORE_DOMAIN_ORE_BOUNDARY_DECIMAL_HPP
#define ORES_ORE_CORE_DOMAIN_ORE_BOUNDARY_DECIMAL_HPP

#include "ores.ore.core/domain/domain_xsd.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <type_traits>

/**
 * @file ore_boundary_decimal.hpp
 * @brief The exact decimal a money leaf's own digits state.
 *
 * An ORE document states a rate, a spread or an amount as an xs:float. The
 * binding keeps the document's digits beside the parsed float, so a money
 * quantity is the exact decimal the document wrote rather than the binary
 * float nearest to it. Money at rest is exact decimal; this header is the
 * boundary where the document's digits enter.
 *
 * A leaf with no lexical form was set in code and has only its float, so the
 * value falls back to that float's shortest round-tripping spelling. A leaf
 * whose text is not a decimal -- INF, NaN, or more significant digits than the
 * representation holds -- falls back the same way, and a value that is not
 * finite has no decimal at all, so it yields zero instead of throwing across
 * the mapping seam.
 */

namespace ores::ore::domain {

/**
 * @brief Reads the exact decimal a floating-point binding carries.
 *
 * @details A non-empty lexical form wins and is parsed with
 * from_string(); every failure falls back to from_double() on the parsed
 * value. The template is constrained to floating-point leaves because only
 * those carry a lexical form.
 */
template <typename T>
ores::utility::decimal::decimal exact_decimal(const xsd::base<T>& v) {
    static_assert(std::is_floating_point_v<T>,
                  "exact_decimal reads the lexical form, which only a floating-point leaf carries");
    const std::string& lex = v.lexical();
    if (!lex.empty()) {
        if (auto parsed = ores::utility::decimal::decimal::from_string(lex))
            return *parsed;
    }
    if (auto parsed = ores::utility::decimal::decimal::from_double(static_cast<double>(v)))
        return *parsed;
    return ores::utility::decimal::decimal{};
}

/**
 * @brief Reads the exact decimal a computed double states.
 *
 * @details For a money value that no binding carried, such as a value computed
 * inside the mapper. The double's shortest round-tripping spelling is the
 * value.
 */
inline ores::utility::decimal::decimal exact_decimal(double v) {
    if (auto parsed = ores::utility::decimal::decimal::from_double(v))
        return *parsed;
    return ores::utility::decimal::decimal{};
}

}

#endif
