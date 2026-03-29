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
#ifndef ORES_TRADING_GENERATOR_COMPOSITE_LEG_GENERATOR_HPP
#define ORES_TRADING_GENERATOR_COMPOSITE_LEG_GENERATOR_HPP

#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.trading.api/domain/composite_leg.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace ores::trading::generator {

/**
 * @brief Generates a synthetic composite_leg for the given instrument_id.
 */
domain::composite_leg generate_synthetic_composite_leg(
    const boost::uuids::uuid& instrument_id,
    int leg_sequence,
    utility::generation::generation_context& ctx);

/**
 * @brief Generates a synthetic pair of composite legs for an instrument.
 */
std::vector<domain::composite_leg>
generate_synthetic_composite_legs(const boost::uuids::uuid& instrument_id,
    utility::generation::generation_context& ctx);

}

#endif
