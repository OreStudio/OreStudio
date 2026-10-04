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
#ifndef ORES_TRADING_DOMAIN_BOOKING_NATURE_HPP
#define ORES_TRADING_DOMAIN_BOOKING_NATURE_HPP

#include <optional>
#include <ostream>
#include <stdexcept>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Whether anything happened to cause a booking.
 *
 * The values match the codes in @c ores_trading_booking_nature_types_tbl, which
 * a trade anchor references with a database foreign key.
 */
enum class booking_nature {
    actual,      ///< The firm did a deal.
    test,        ///< The booking exercises the system.
    hypothetical ///< The booking answers a question.
};

/**
 * @brief Convert a booking_nature to its code.
 *
 * Throws @c std::invalid_argument on an out-of-range value.
 */
[[nodiscard]] inline std::string_view to_string(booking_nature v) {
    switch (v) {
        case booking_nature::actual:
            return "actual";
        case booking_nature::test:
            return "test";
        case booking_nature::hypothetical:
            return "hypothetical";
    }
    throw std::invalid_argument("Out-of-range booking_nature");
}

/**
 * @brief Stream a booking_nature using its code.
 */
inline std::ostream& operator<<(std::ostream& s, booking_nature v) {
    return s << to_string(v);
}

/**
 * @brief Parse a booking_nature from its code.
 *
 * Returns @c std::nullopt for an unrecognised code.
 */
[[nodiscard]] inline std::optional<booking_nature> booking_nature_from_string(std::string_view sv) {
    if (sv == "actual")
        return booking_nature::actual;
    if (sv == "test")
        return booking_nature::test;
    if (sv == "hypothetical")
        return booking_nature::hypothetical;
    return std::nullopt;
}

/**
 * @brief Parse a booking nature from a shell command token.
 *
 * The shell command-token reader finds this overload by argument-dependent
 * lookup, so the shell header needs no include of this one.
 */
[[nodiscard]] inline std::optional<booking_nature> parse_token(std::string_view sv,
                                                               booking_nature) {
    return booking_nature_from_string(sv);
}

}

#endif
