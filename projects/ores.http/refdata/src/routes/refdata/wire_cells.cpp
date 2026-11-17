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
#include "ores.http/routes/refdata/wire_cells.hpp"
#include <charconv>
#include <string>

namespace ores::http::routes::refdata::document {

cell cell_at(const row& source, std::string_view column) {
    const auto found = source.find(column);
    return found == source.end() ? cell() : found->second;
}

std::string text_of(const row& source, std::string_view column) {
    const auto stated = cell_at(source, column);
    return stated ? *stated : std::string();
}

int number_of(const row& source, std::string_view column) {
    const auto stated = text_of(source, column);
    int number = 0;
    const auto read = std::from_chars(stated.data(), stated.data() + stated.size(), number);
    return read.ec == std::errc() ? number : 0;
}

}
