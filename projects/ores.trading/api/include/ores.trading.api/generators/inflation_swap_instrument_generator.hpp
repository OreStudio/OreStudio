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
#ifndef ORES_TRADING_GENERATORS_INFLATION_SWAP_INSTRUMENT_GENERATOR_HPP
#define ORES_TRADING_GENERATORS_INFLATION_SWAP_INSTRUMENT_GENERATOR_HPP

#include "ores.trading.api/domain/inflation_swap_instrument.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <vector>

namespace ores::trading::generators {

/**
 * @brief Generates a synthetic inflation_swap_instrument.
 */
ORES_TRADING_API_EXPORT domain::inflation_swap_instrument
generate_synthetic_inflation_swap_instrument(utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic inflation_swap_instruments.
 */
ORES_TRADING_API_EXPORT std::vector<domain::inflation_swap_instrument>
generate_synthetic_inflation_swap_instruments(std::size_t n,
                                              utility::generation::generation_context& ctx);

}

#endif
