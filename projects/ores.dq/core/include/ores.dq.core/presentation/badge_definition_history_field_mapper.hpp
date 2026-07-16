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
#ifndef ORES_DQ_CORE_PRESENTATION_BADGE_DEFINITION_HISTORY_FIELD_MAPPER_HPP
#define ORES_DQ_CORE_PRESENTATION_BADGE_DEFINITION_HISTORY_FIELD_MAPPER_HPP

#include "ores.diff/domain/field_value.hpp"
#include "ores.dq.api/domain/badge_definition.hpp"
#include "ores.dq.core/export.hpp"
#include <vector>

namespace ores::dq::presentation {

/**
 * @brief Renders a badge_definition to an ordered field list for
 * history-diff display. One line per field, in mapper order; no
 * runtime reflection.
 */
[[nodiscard]] ORES_DQ_CORE_EXPORT std::vector<ores::diff::domain::field_value>
render_badge_definition_fields(const domain::badge_definition& v);

}

#endif
