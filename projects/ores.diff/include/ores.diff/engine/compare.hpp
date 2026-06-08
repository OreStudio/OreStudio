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
#ifndef ORES_DIFF_ENGINE_COMPARE_HPP
#define ORES_DIFF_ENGINE_COMPARE_HPP

#include "ores.diff/domain/diff_result.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.diff/export.hpp"
#include <vector>

namespace ores::diff::engine {

/**
 * @brief Computes the field-level differences between two versions.
 *
 * Fields are keyed by name. A field present in both lists with a
 * different value yields an entry with both sides; a field only in
 * current (added) yields an entry with an empty old_value; a field
 * only in previous (removed) yields an entry with an empty new_value.
 * Unchanged fields yield nothing.
 *
 * Entry order follows the current list's order, with removed fields
 * appended last in the previous list's order. When a name appears
 * more than once in a list, the first occurrence wins.
 *
 * The model is flat by design: nested entities flatten in the mapper,
 * encoding topology in path-style display names (e.g. "Legs[1] /
 * Notional"). The engine never interprets names.
 *
 * @param previous The older version's fields, in mapper order.
 * @param current The newer version's fields, in mapper order.
 */
ORES_DIFF_EXPORT domain::diff_result compute(const std::vector<domain::field_value>& previous,
                                             const std::vector<domain::field_value>& current);

}

#endif
