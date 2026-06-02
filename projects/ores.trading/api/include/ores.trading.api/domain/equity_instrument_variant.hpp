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
#ifndef ORES_TRADING_DOMAIN_EQUITY_INSTRUMENT_VARIANT_HPP
#define ORES_TRADING_DOMAIN_EQUITY_INSTRUMENT_VARIANT_HPP

#include <variant>
#include "ores.trading.api/domain/equity_option_instrument.hpp"
#include "ores.trading.api/domain/equity_digital_option_instrument.hpp"
#include "ores.trading.api/domain/equity_barrier_option_instrument.hpp"
#include "ores.trading.api/domain/equity_asian_option_instrument.hpp"
#include "ores.trading.api/domain/equity_forward_instrument.hpp"
#include "ores.trading.api/domain/equity_variance_swap_instrument.hpp"
#include "ores.trading.api/domain/equity_swap_instrument.hpp"
#include "ores.trading.api/domain/equity_accumulator_instrument.hpp"
#include "ores.trading.api/domain/equity_position_instrument.hpp"

namespace ores::trading::domain {

using equity_instrument_variant = std::variant<
    equity_option_instrument,
    equity_digital_option_instrument,
    equity_barrier_option_instrument,
    equity_asian_option_instrument,
    equity_forward_instrument,
    equity_variance_swap_instrument,
    equity_swap_instrument,
    equity_accumulator_instrument,
    equity_position_instrument
>;

} // namespace ores::trading::domain

#endif
