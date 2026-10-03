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
#ifndef ORES_TRADING_DOMAIN_ENTRY_CHANNEL_HPP
#define ORES_TRADING_DOMAIN_ENTRY_CHANNEL_HPP

#include <optional>
#include <ostream>
#include <stdexcept>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief How a trade reached the firm's books.
 *
 * The values match the codes in @c ores_trading_entry_channel_types_tbl, which
 * a trade anchor references with a database foreign key.
 */
enum class entry_channel {
    manual,     ///< A user captured the trade.
    stp,        ///< Straight-through processing from an upstream system.
    ecn,        ///< An electronic communication network.
    allocation  ///< An allocation split from a block trade.
};

/**
 * @brief Convert an entry_channel to its code.
 *
 * Throws @c std::invalid_argument on an out-of-range value.
 */
[[nodiscard]] inline std::string_view to_string(entry_channel v) {
    switch (v) {
        case entry_channel::manual:
            return "manual";
        case entry_channel::stp:
            return "stp";
        case entry_channel::ecn:
            return "ecn";
        case entry_channel::allocation:
            return "allocation";
    }
    throw std::invalid_argument("Out-of-range entry_channel");
}

/**
 * @brief Stream an entry_channel using its code.
 */
inline std::ostream& operator<<(std::ostream& s, entry_channel v) {
    return s << to_string(v);
}

/**
 * @brief Parse an entry_channel from its code.
 *
 * Returns @c std::nullopt for an unrecognised code.
 */
[[nodiscard]] inline std::optional<entry_channel> entry_channel_from_string(std::string_view sv) {
    if (sv == "manual")
        return entry_channel::manual;
    if (sv == "stp")
        return entry_channel::stp;
    if (sv == "ecn")
        return entry_channel::ecn;
    if (sv == "allocation")
        return entry_channel::allocation;
    return std::nullopt;
}

/**
 * @brief Parse an entry channel from a shell command token.
 *
 * The shell command-token reader finds this overload by argument-dependent
 * lookup, so the shell header needs no include of this one.
 */
[[nodiscard]] inline std::optional<entry_channel> parse_token(std::string_view sv, entry_channel) {
    return entry_channel_from_string(sv);
}

}

#endif
