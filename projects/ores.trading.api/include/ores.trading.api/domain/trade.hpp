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
#ifndef ORES_TRADING_DOMAIN_TRADE_HPP
#define ORES_TRADING_DOMAIN_TRADE_HPP

#include <rfl/Flatten.hpp>
#include "ores.trading.api/domain/trade_identity.hpp"
#include "ores.trading.api/domain/trade_parties.hpp"
#include "ores.trading.api/domain/trade_classification.hpp"
#include "ores.trading.api/domain/trade_lifecycle.hpp"
#include "ores.trading.api/domain/trade_audit.hpp"

namespace ores::trading::domain {

/**
 * @brief Trade capturing FpML Trade Header properties.
 *
 * Temporal trade record. Each lifecycle event (New, Amendment, Novation,
 * etc.) creates a new temporal row for the same trade id. The internal party
 * is derived from book_id via books.party_id.
 *
 * Composed of five sub-structs via rfl::Flatten so that the JSON wire format
 * remains flat while each sub-struct stays small enough to avoid MSVC C1202
 * (recursive template dependency context too complex) in rfl's O(n²)
 * field-uniqueness check.
 */
struct trade final {
    rfl::Flatten<trade_identity> identity;
    rfl::Flatten<trade_parties> parties;
    rfl::Flatten<trade_classification> classification;
    rfl::Flatten<trade_lifecycle> lifecycle;
    rfl::Flatten<trade_audit> audit;
};

}

#endif
