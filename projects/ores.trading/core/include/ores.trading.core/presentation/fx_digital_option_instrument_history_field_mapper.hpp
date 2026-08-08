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
#ifndef ORES_TRADING_CORE_PRESENTATION_FX_DIGITAL_OPTION_INSTRUMENT_HISTORY_FIELD_MAPPER_HPP
#define ORES_TRADING_CORE_PRESENTATION_FX_DIGITAL_OPTION_INSTRUMENT_HISTORY_FIELD_MAPPER_HPP

#include "ores.diff/domain/field_value.hpp"
#include "ores.trading.api/domain/fx_digital_option_instrument.hpp"
#include "ores.trading.core/export.hpp"
#include <vector>

namespace ores::trading::presentation {

/**
 * @brief Renders a fx_digital_option_instrument to an ordered field list for
 * history-diff display. One line per field, in mapper order; no
 * runtime reflection.
 */
[[nodiscard]] ORES_TRADING_CORE_EXPORT std::vector<ores::diff::domain::field_value>
render_fx_digital_option_instrument_fields(const domain::fx_digital_option_instrument& v);

}

#endif
