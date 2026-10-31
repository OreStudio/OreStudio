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
#include "output_csv.hpp"
#include <charconv>

namespace ores::ore::results::output_csv {

bool is_null(std::string_view text) {
    return text.empty() || text == "#N/A" || text == "#NA" || text == "N/A" || text == "null" ||
           text == "NULL" || text == "NaN" || text == "-nan";
}

std::vector<std::string> fields_of(std::string_view line) {
    std::vector<std::string> fields;
    std::string field;
    bool quoted = false;
    for (const auto c : line) {
        if (c == '"') {
            quoted = !quoted;
        } else if (c == ',' && !quoted) {
            fields.push_back(field);
            field.clear();
        } else if (c != '\r') {
            field.push_back(c);
        }
    }
    fields.push_back(field);
    return fields;
}

bool figure_of(std::string_view text, double& figure) {
    if (text.empty())
        return false;
    const auto* first = text.data();
    const auto* last = text.data() + text.size();
    const auto parsed = std::from_chars(first, last, figure);
    return parsed.ec == std::errc() && parsed.ptr == last;
}

std::string joined(const std::vector<std::string>& names) {
    std::string out;
    for (const auto& name : names) {
        if (!out.empty())
            out += ", ";
        out += name;
    }
    return out;
}

}
