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
#ifndef ORES_TRADING_DOMAIN_ORIGIN_HPP
#define ORES_TRADING_DOMAIN_ORIGIN_HPP

#include <optional>
#include <ostream>
#include <stdexcept>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Which system first wrote a trade down.
 *
 * The values match the codes in @c ores_trading_origin_types_tbl, which a trade
 * anchor references with a database foreign key. Origin is provenance and not
 * economics: a trade imported twice is one trade.
 */
enum class origin {
    ux,         ///< A person entered the trade at a screen.
    api,        ///< An external caller wrote it through the API.
    ore_import, ///< The ORE XML import wrote it.
    synthetic   ///< The trade population generator wrote it.
};

/**
 * @brief Convert an origin to its code.
 *
 * Throws @c std::invalid_argument on an out-of-range value.
 */
[[nodiscard]] inline std::string_view to_string(origin v) {
    switch (v) {
        case origin::ux:
            return "ux";
        case origin::api:
            return "api";
        case origin::ore_import:
            return "ore_import";
        case origin::synthetic:
            return "synthetic";
    }
    throw std::invalid_argument("Out-of-range origin");
}

/**
 * @brief Stream an origin using its code.
 */
inline std::ostream& operator<<(std::ostream& s, origin v) {
    return s << to_string(v);
}

/**
 * @brief Parse an origin from its code.
 *
 * Returns @c std::nullopt for an unrecognised code.
 */
[[nodiscard]] inline std::optional<origin> origin_from_string(std::string_view sv) {
    if (sv == "ux")
        return origin::ux;
    if (sv == "api")
        return origin::api;
    if (sv == "ore_import")
        return origin::ore_import;
    if (sv == "synthetic")
        return origin::synthetic;
    return std::nullopt;
}

/**
 * @brief Parse an origin from a shell command token.
 *
 * The shell command-token reader finds this overload by argument-dependent
 * lookup, so the shell header needs no include of this one.
 */
[[nodiscard]] inline std::optional<origin> parse_token(std::string_view sv, origin) {
    return origin_from_string(sv);
}

}

#endif
