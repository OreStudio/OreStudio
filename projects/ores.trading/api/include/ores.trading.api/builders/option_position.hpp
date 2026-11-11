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
#ifndef ORES_TRADING_API_BUILDERS_OPTION_POSITION_HPP
#define ORES_TRADING_API_BUILDERS_OPTION_POSITION_HPP

#include <string>

namespace ores::trading::builders {

/// Whether the trade holds the option or wrote it.
enum class option_position { long_option, short_option };

[[nodiscard]] inline std::string long_short_of(option_position position) {
    return position == option_position::long_option ? "Long" : "Short";
}

/// Whether the trade settles in cash or by delivery.
enum class settlement { cash, physical };

[[nodiscard]] inline std::string settlement_of(settlement settles) {
    return settles == settlement::cash ? "Cash" : "Physical";
}

/// Which way an option pays: on a rise or on a fall.
enum class option_kind { call, put };

[[nodiscard]] inline std::string option_type_of(option_kind kind) {
    return kind == option_kind::call ? "Call" : "Put";
}

}

#endif
