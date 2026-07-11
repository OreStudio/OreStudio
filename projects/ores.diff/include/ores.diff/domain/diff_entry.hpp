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
#ifndef ORES_DIFF_DOMAIN_DIFF_ENTRY_HPP
#define ORES_DIFF_DOMAIN_DIFF_ENTRY_HPP

#include "ores.diff/domain/diff_span.hpp"
#include <string>
#include <vector>

namespace ores::diff::domain {

/**
 * @brief One changed field: its name and both rendered values.
 *
 * A field added in the current version has an empty old_value; a
 * field removed from the current version has an empty new_value.
 */
struct diff_entry final {
    /**
     * @brief Display name of the field, e.g. "ISO Code".
     */
    std::string field_name;

    /**
     * @brief The previous version's rendered value; empty when the
     * field was added.
     */
    std::string old_value;

    /**
     * @brief The current version's rendered value; empty when the
     * field was removed.
     */
    std::string new_value;

    /**
     * @brief Byte ranges into old_value that changed relative to
     * new_value; empty when the field was added (old_value is
     * already empty) or when the whole value changed with no
     * meaningful sub-range to highlight.
     */
    std::vector<diff_span> old_spans;

    /**
     * @brief Byte ranges into new_value that changed relative to
     * old_value; empty when the field was removed (new_value is
     * already empty) or when the whole value changed with no
     * meaningful sub-range to highlight.
     */
    std::vector<diff_span> new_spans;

    friend bool operator==(const diff_entry&, const diff_entry&) = default;
};

}

#endif
