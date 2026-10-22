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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_PG_ARRAY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_PG_ARRAY_HPP

#include <string>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief The text of a PostgreSQL array literal that holds @p elements.
 *
 * A read takes its parameters as text, so a list travels as one literal that the statement casts
 * to the array type. Each element is quoted, with a backslash before a quote or a backslash.
 */
[[nodiscard]] inline std::string pg_array(const std::vector<std::string>& elements) {
    std::string out = "{";
    for (const auto& element : elements) {
        if (out.size() > 1)
            out += ',';
        out += '"';
        for (const char c : element) {
            if (c == '"' || c == '\\')
                out += '\\';
            out += c;
        }
        out += '"';
    }
    out += '}';
    return out;
}

}

#endif
