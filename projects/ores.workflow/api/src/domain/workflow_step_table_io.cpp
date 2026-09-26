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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_table_io.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.workflow.api/domain/workflow_step_table_io.hpp"
#include "ores.workflow.api/domain/workflow_step_table.hpp"
#include <ostream>

namespace ores::workflow::domain {

namespace {

void print_workflow_step_table(std::ostream& s, const std::vector<workflow_step>& v) {
    s << std::endl << convert_to_table(v) << std::endl;
}

}

std::ostream& operator<<(std::ostream& s, const std::vector<workflow_step>& v) {
    print_workflow_step_table(s, v);
    return s;
}

}
