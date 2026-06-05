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
#ifndef ORES_DIFF_DOMAIN_FIELD_VALUE_HPP
#define ORES_DIFF_DOMAIN_FIELD_VALUE_HPP

#include <string>

namespace ores::diff::domain {

/**
 * @brief One field of an entity version, rendered for display.
 *
 * Per-entity field mappers produce ordered lists of these; the
 * rendering decision (how a date, amount or flag becomes a string)
 * belongs to the mapper, not to the diff engine.
 */
struct field_value final {
    /**
     * @brief Display name of the field, e.g. "ISO Code".
     */
    std::string name;

    /**
     * @brief The field's value rendered as a display string.
     */
    std::string value;

    friend bool operator==(const field_value&, const field_value&) = default;
};

}

#endif
