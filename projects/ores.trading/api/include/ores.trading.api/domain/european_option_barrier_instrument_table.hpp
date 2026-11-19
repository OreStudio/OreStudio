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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_table.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_EUROPEAN_OPTION_BARRIER_INSTRUMENT_TABLE_HPP
#define ORES_TRADING_API_DOMAIN_EUROPEAN_OPTION_BARRIER_INSTRUMENT_TABLE_HPP

#include "ores.trading.api/domain/european_option_barrier_instrument.hpp"
#include "ores.trading.api/export.hpp"
#include <string>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief Converts european_option_barrier_instruments to the table format.
 */
ORES_TRADING_API_EXPORT std::string
convert_to_table(const std::vector<european_option_barrier_instrument>& v);

}

#endif
