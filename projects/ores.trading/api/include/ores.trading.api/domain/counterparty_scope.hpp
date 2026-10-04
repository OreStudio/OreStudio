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
#ifndef ORES_TRADING_DOMAIN_COUNTERPARTY_SCOPE_HPP
#define ORES_TRADING_DOMAIN_COUNTERPARTY_SCOPE_HPP

#include <optional>
#include <ostream>
#include <stdexcept>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Who the firm faces on a trade.
 *
 * The values match the codes in @c ores_trading_counterparty_scope_types_tbl,
 * which a trade anchor references with a database foreign key.
 */
enum class counterparty_scope {
    external,     ///< A party outside the group.
    inter_entity, ///< Another legal entity of the same group.
    intra_entity  ///< Another book in the same legal entity and branch.
};

/**
 * @brief Convert a counterparty_scope to its code.
 *
 * Throws @c std::invalid_argument on an out-of-range value.
 */
[[nodiscard]] inline std::string_view to_string(counterparty_scope v) {
    switch (v) {
        case counterparty_scope::external:
            return "external";
        case counterparty_scope::inter_entity:
            return "inter_entity";
        case counterparty_scope::intra_entity:
            return "intra_entity";
    }
    throw std::invalid_argument("Out-of-range counterparty_scope");
}

/**
 * @brief Stream a counterparty_scope using its code.
 */
inline std::ostream& operator<<(std::ostream& s, counterparty_scope v) {
    return s << to_string(v);
}

/**
 * @brief Parse a counterparty_scope from its code.
 *
 * Returns @c std::nullopt for an unrecognised code.
 */
[[nodiscard]] inline std::optional<counterparty_scope>
counterparty_scope_from_string(std::string_view sv) {
    if (sv == "external")
        return counterparty_scope::external;
    if (sv == "inter_entity")
        return counterparty_scope::inter_entity;
    if (sv == "intra_entity")
        return counterparty_scope::intra_entity;
    return std::nullopt;
}

/**
 * @brief Parse a counterparty scope from a shell command token.
 *
 * The shell command-token reader finds this overload by argument-dependent
 * lookup, so the shell header needs no include of this one.
 */
[[nodiscard]] inline std::optional<counterparty_scope> parse_token(std::string_view sv,
                                                                   counterparty_scope) {
    return counterparty_scope_from_string(sv);
}

}

#endif
