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
#ifndef ORES_SYNTHETIC_CORE_PRESENTATION_IR_CURVE_TEMPLATE_ENTRY_HISTORY_FIELD_MAPPER_HPP
#define ORES_SYNTHETIC_CORE_PRESENTATION_IR_CURVE_TEMPLATE_ENTRY_HISTORY_FIELD_MAPPER_HPP

#include "ores.diff/domain/field_value.hpp"
#include "ores.synthetic.api/domain/ir_curve_template_entry.hpp"
#include "ores.synthetic.core/export.hpp"
#include <vector>

namespace ores::synthetic::presentation {

/**
 * @brief Renders a ir_curve_template_entry to an ordered field list for
 * history-diff display. One line per field, in mapper order; no
 * runtime reflection.
 */
[[nodiscard]] ORES_SYNTHETIC_CORE_EXPORT std::vector<ores::diff::domain::field_value>
render_ir_curve_template_entry_fields(const domain::ir_curve_template_entry& v);

}

#endif
