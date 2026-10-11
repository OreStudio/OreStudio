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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_ECONOMIC_DIGEST_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_ECONOMIC_DIGEST_HPP

#include "ores.trading.api/domain/economic_digest.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include <string>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief The SHA-256 digest of a trade's economics: its own economic fields
 * plus one digest per component, folded in the order given.
 *
 * The trade's own fields keep the exclusion and canonicalisation rules of
 * @c economic_digest, so a change to an identity or audit field leaves this
 * digest alone. Every value in the fold is framed by length, including the
 * component count, so two component lists frame to the same text only when
 * they hold the same digests in the same order. Reordering, adding, removing
 * or amending a component therefore changes the result, and an empty list is
 * distinct from a non-empty one.
 *
 * The fold is pure: it reads no database and no clock, so the same inputs
 * always give the same digest.
 *
 * @param t The trade whose own economic fields are digested.
 * @param component_digests One digest per component, in the fold's fixed order.
 * @return The lowercase hex SHA-256 digest.
 */
inline std::string trade_economic_digest(const trade& t,
                                         const std::vector<std::string>& component_digests) {
    std::string canonical = detail::frame(economic_digest(t));
    canonical.append(detail::frame(std::to_string(component_digests.size())));
    canonical.push_back('{');
    for (const auto& component_digest : component_digests)
        canonical.append(detail::frame(component_digest));
    canonical.push_back('}');
    return ores::utility::crypto::sha256::hex_digest(canonical);
}

}

#endif
