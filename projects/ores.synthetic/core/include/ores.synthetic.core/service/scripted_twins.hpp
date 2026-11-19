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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_SCRIPTED_TWINS_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_SCRIPTED_TWINS_HPP

#include "ores.synthetic.api/domain/trade_population_plan.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.trading.api/domain/instrument_batch.hpp"
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief The values a recipe source names in a native trade's rows, as text.
 *
 * A source is =product.<column>= for a column of the row whose trade type is
 * the native one, =<rows>[<role>].<column>= for a column of the rows under a
 * role, =<rows>.<column>= for a column of every row of the array, or a quoted
 * literal; =a or b= takes =b= when =a= names no value. A row's columns are found
 * by the names its wire fields state, so no source needs code of its own.
 */
ORES_SYNTHETIC_CORE_EXPORT std::vector<std::string>
twin_source_values(const ores::trading::domain::instrument_batch& batch,
                   std::string_view trade_type_code,
                   std::string_view source);

/**
 * @brief The generic ScriptedTrade twin of a native slot, on the same terms.
 *
 * Nothing when the slot's type has no recipe. The twin is a slot of its own,
 * with an id derived from the native trade's, whose rows the scripted trade
 * builder states from the recipe read over the native rows.
 */
ORES_SYNTHETIC_CORE_EXPORT std::optional<domain::trade_population_slot>
scripted_twin_of(const domain::trade_population_slot& native);

/// Appends the scripted twin of every slot whose type has a recipe.
ORES_SYNTHETIC_CORE_EXPORT void add_scripted_twins(domain::trade_population_plan& plan);

}

#endif
