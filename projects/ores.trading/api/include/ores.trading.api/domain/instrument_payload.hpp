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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_PAYLOAD_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_PAYLOAD_HPP

#include "ores.trading.api/domain/trade_instrument.hpp"
#include "ores.trading.api/export.hpp"
#include <string>

namespace ores::trading::domain {

/**
 * @brief Carries one instrument across a boundary that cannot name a variant.
 *
 * reflect-cpp writes a std::variant without naming the active alternative, and
 * reads it back by trying the alternatives in declaration order. std::monostate
 * is the first alternative of trade_instrument and parses from any payload, so
 * an untagged instrument decodes as monostate and is lost in silence.
 *
 * Naming the alternative on the wire is not open to us either.
 * rfl::AddTagsToVariants builds an rfl::Literal over every field name reachable
 * through the variant, 502 of them, which exceeds the fold-expression nesting
 * limit on macOS and the recursive-type limit on MSVC. This payload names the
 * alternative itself, and encodes the leaf with a type the reader knows.
 *
 * A monostate instrument has an empty type and an empty body.
 */
struct instrument_payload {
    std::string type;
    std::string body;
};

/**
 * @brief Encodes an instrument, naming its leaf type in the payload.
 */
ORES_TRADING_API_EXPORT instrument_payload encode_instrument(const trade_instrument& instrument);

/**
 * @brief Decodes a payload back into an instrument.
 *
 * Returns a monostate instrument when the payload is empty, when the type is
 * unrecognised, or when the body does not parse as that type. A loss is
 * reported rather than guessed at.
 */
ORES_TRADING_API_EXPORT trade_instrument decode_instrument(const instrument_payload& payload);

}

#endif
