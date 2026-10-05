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
#ifndef ORES_INBOX_API_DOMAIN_DELIVERY_OUTCOME_HPP
#define ORES_INBOX_API_DOMAIN_DELIVERY_OUTCOME_HPP

#include <optional>
#include <ostream>
#include <stdexcept>
#include <string_view>

namespace ores::inbox::domain {

/**
 * @brief What came of one attempt to deliver a notification.
 *
 * The values match the codes in @c ores_inbox_delivery_outcome_types_tbl,
 * which a delivery references with a database foreign key.
 */
enum class delivery_outcome {
    pending,   ///< The attempt is queued and has not finished.
    delivered, ///< The channel took the notification.
    failed     ///< The channel refused it; the delivery says why.
};

/**
 * @brief Convert a delivery_outcome to its code.
 *
 * Throws @c std::invalid_argument on an out-of-range value.
 */
[[nodiscard]] inline std::string_view to_string(delivery_outcome v) {
    switch (v) {
        case delivery_outcome::pending:
            return "pending";
        case delivery_outcome::delivered:
            return "delivered";
        case delivery_outcome::failed:
            return "failed";
    }
    throw std::invalid_argument("Out-of-range delivery_outcome");
}

/**
 * @brief Stream a delivery_outcome using its code.
 */
inline std::ostream& operator<<(std::ostream& s, delivery_outcome v) {
    return s << to_string(v);
}

/**
 * @brief Parse a delivery_outcome from its code.
 *
 * Returns @c std::nullopt for an unrecognised code.
 */
[[nodiscard]] inline std::optional<delivery_outcome>
delivery_outcome_from_string(std::string_view sv) {
    if (sv == "pending")
        return delivery_outcome::pending;
    if (sv == "delivered")
        return delivery_outcome::delivered;
    if (sv == "failed")
        return delivery_outcome::failed;
    return std::nullopt;
}

/**
 * @brief Parse a delivery outcome from a shell command token.
 *
 * The shell command-token reader finds this overload by argument-dependent
 * lookup, so the shell header needs no include of this one.
 */
[[nodiscard]] inline std::optional<delivery_outcome> parse_token(std::string_view sv,
                                                                 delivery_outcome) {
    return delivery_outcome_from_string(sv);
}

}

#endif
