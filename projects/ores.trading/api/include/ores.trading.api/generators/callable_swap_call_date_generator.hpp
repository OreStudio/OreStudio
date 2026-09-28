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
 * Template: cpp_domain_type_generator.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_GENERATORS_CALLABLE_SWAP_CALL_DATE_GENERATOR_HPP
#define ORES_TRADING_API_GENERATORS_CALLABLE_SWAP_CALL_DATE_GENERATOR_HPP

#include "ores.trading.api/domain/callable_swap_call_date.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <vector>

namespace ores::trading::generators {

/**
 * @brief Generates a synthetic callable_swap_call_date.
 */
ORES_TRADING_API_EXPORT domain::callable_swap_call_date
generate_synthetic_callable_swap_call_date(utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic callable_swap_call_dates.
 */
ORES_TRADING_API_EXPORT std::vector<domain::callable_swap_call_date>
generate_synthetic_callable_swap_call_dates(std::size_t n,
                                            utility::generation::generation_context& ctx);

}

#endif
