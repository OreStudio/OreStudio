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
#ifndef ORES_QT_PARSE_TRADE_INSTRUMENT_HPP
#define ORES_QT_PARSE_TRADE_INSTRUMENT_HPP

#include "ores.qt.headless/export.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include <optional>
#include <string>

namespace ores::qt {

struct IInstrumentFormPopulator;

/**
 * @brief Two-phase JSON parse for a get_trade_instrument_response payload.
 *
 * Phase 1 reads the response envelope (trade + success/message); Phase 2
 * dispatches on (product_type, trade_type) to read the concrete leaf
 * instrument struct and calls populator.populate() with the result.
 *
 * No rfl::AddTagsToVariants is used. No trade_instrument variant is
 * constructed. Returns the trade on success, nullopt on any failure.
 */
ORES_QT_HEADLESS_API std::optional<trading::domain::trade>
parse_trade_instrument(const std::string& raw, IInstrumentFormPopulator& populator);

} // namespace ores::qt

#endif
