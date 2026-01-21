/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.dq/domain/publication_mode.hpp"

namespace ores::dq::domain {

std::string to_string(publication_mode mode) {
    switch (mode) {
    case publication_mode::upsert:
        return "upsert";
    case publication_mode::insert_only:
        return "insert_only";
    case publication_mode::replace_all:
        return "replace_all";
    }
    return "unknown";
}

std::optional<publication_mode> publication_mode_from_string(const std::string& s) {
    if (s == "upsert") {
        return publication_mode::upsert;
    } else if (s == "insert_only") {
        return publication_mode::insert_only;
    } else if (s == "replace_all") {
        return publication_mode::replace_all;
    }
    return std::nullopt;
}

std::ostream& operator<<(std::ostream& s, publication_mode mode) {
    return s << to_string(mode);
}

}
