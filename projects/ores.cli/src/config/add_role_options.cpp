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
#include "ores.cli/config/add_role_options.hpp"

#include <ostream>

namespace ores::cli::config {

std::ostream& operator<<(std::ostream& s, const add_role_options& v) {
    s << "{ name: " << v.name
      << ", description: " << v.description
      << ", recorded_by: " << v.recorded_by;

    if (v.change_reason_code) s << ", change_reason_code: " << *v.change_reason_code;
    if (v.change_commentary) s << ", change_commentary: " << *v.change_commentary;

    s << ", permission_codes: [";
    for (size_t i = 0; i < v.permission_codes.size(); ++i) {
        if (i > 0) s << ", ";
        s << v.permission_codes[i];
    }
    s << "] }";
    return s;
}

}
