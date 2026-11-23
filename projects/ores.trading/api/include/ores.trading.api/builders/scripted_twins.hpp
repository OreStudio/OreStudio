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
#ifndef ORES_TRADING_API_BUILDERS_SCRIPTED_TWINS_HPP
#define ORES_TRADING_API_BUILDERS_SCRIPTED_TWINS_HPP

#include "ores.trading.api/domain/instrument_batch.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/export.hpp"
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::builders {

/**
 * @brief A twin recipe that cannot be read over a native trade's rows: a source
 * naming no value, a schedule that is missing, a transform with no code.
 */
class ORES_TRADING_API_EXPORT scripted_twin_error : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

/**
 * @brief The values a recipe source names in a native trade's rows, as text.
 *
 * A source is =product.<column>= for a column of the row whose trade type is
 * the native one, =<rows>[<role>].<column>= for a column of the rows under a
 * role, =<rows>.<column>= for a column of every row of the array, or a quoted
 * literal; =a or b= takes =b= when =a= names no value, and =a and b= lists the
 * values of =a= then those of =b=. A row's columns are found
 * by the names its wire fields state, so no source needs code of its own.
 */
ORES_TRADING_API_EXPORT std::vector<std::string>
twin_source_values(const domain::instrument_batch& batch,
                   std::string_view trade_type_code,
                   std::string_view source);

/**
 * @brief One scripted form of a native trade: the library script it runs,
 * whether it is the native's twin or a script with no element of its own that
 * the native carries, and the scripted instrument its recipe states.
 */
struct scripted_form final {
    std::string script_name;
    bool twin = false;
    domain::scripted_instrument_data data;
};

/**
 * @brief Every scripted form of a native trade of a type, its twin first, each
 * read by its recipe over the native's rows. None when the type has no recipe.
 *
 * @throws scripted_twin_error when a recipe cannot be read over the rows, or
 * the scripted trade builder refuses what it reads.
 */
ORES_TRADING_API_EXPORT std::vector<scripted_form>
scripted_forms_of(const domain::instrument_batch& instruments, std::string_view trade_type_code);

}

#endif
