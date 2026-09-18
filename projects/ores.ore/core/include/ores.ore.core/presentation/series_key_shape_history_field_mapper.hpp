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
 * Template: cpp_history_field_mapper.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_ORE_CORE_PRESENTATION_SERIES_KEY_SHAPE_HISTORY_FIELD_MAPPER_HPP
#define ORES_ORE_CORE_PRESENTATION_SERIES_KEY_SHAPE_HISTORY_FIELD_MAPPER_HPP

#include "ores.diff/domain/field_value.hpp"
#include "ores.ore.api/domain/series_key_shape.hpp"
#include "ores.ore.core/export.hpp"
#include <vector>

namespace ores::ore::presentation {

/**
 * @brief Renders a series_key_shape to an ordered field list for
 * history-diff display. One line per field, in mapper order; no
 * runtime reflection.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT std::vector<ores::diff::domain::field_value>
render_series_key_shape_fields(const domain::series_key_shape& v);

}

#endif
