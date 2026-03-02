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
#include "ores.reporting/domain/report_definition_table.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>

namespace ores::reporting::domain {

std::string convert_to_table(const std::vector<report_definition>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header
          << "ID" << "Name" << "Type" << "Schedule" << "Concurrency"
          << "Version" << "Modified By" << fort::endr;

    for (const auto& rd : v) {
        table << boost::uuids::to_string(rd.id)
              << rd.name
              << rd.report_type
              << rd.schedule_expression
              << rd.concurrency_policy
              << rd.version
              << rd.modified_by
              << fort::endr;
    }
    return table.to_string();
}

}
