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
#include "ores.dq/domain/treatment_dimension_table_io.hpp"

#include <ostream>
#include "ores.dq/domain/treatment_dimension_table.hpp"

namespace ores::dq::domain {

namespace {

void print_treatment_dimension_table(std::ostream& s,
    const std::vector<treatment_dimension>& v) {
    s << std::endl << convert_to_table(v) << std::endl;
}

}

std::ostream& operator<<(std::ostream& s, const std::vector<treatment_dimension>& v) {
    print_treatment_dimension_table(s, v);
    return s;
}

}
