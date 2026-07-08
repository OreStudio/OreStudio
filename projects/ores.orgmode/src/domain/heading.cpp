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
#include "ores.orgmode/domain/heading.hpp"

namespace ores::orgmode::domain {

namespace {

std::string trim(const std::string& s) {
    const auto begin = s.find_first_not_of(" \t");
    if (begin == std::string::npos)
        return {};
    const auto end = s.find_last_not_of(" \t");
    return s.substr(begin, end - begin + 1);
}

}

std::string join_paragraph_lines(const std::vector<std::string>& lines) {
    std::string out;
    for (const auto& line : lines) {
        const auto trimmed = trim(line);
        if (trimmed.empty())
            continue;
        if (!out.empty())
            out += ' ';
        out += trimmed;
    }
    return out;
}

}
