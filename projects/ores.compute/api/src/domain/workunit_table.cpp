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
#include "ores.compute.api/domain/workunit_table.hpp"

#include <sstream>
#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>

namespace ores::compute::domain {

std::string convert_to_table(const std::vector<workunit>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header
          << "ID" << "Batch ID" << "App Version ID" << "Input URI"
          << "Config URI" << "Priority" << "Target Redundancy"
          << "Modified By" << "Recorded At" << fort::endr;

    for (const auto& wu : v) {
        table << boost::uuids::to_string(wu.id)
              << boost::uuids::to_string(wu.batch_id)
              << boost::uuids::to_string(wu.app_version_id)
              << wu.input_uri << wu.config_uri
              << wu.priority << wu.target_redundancy
              << wu.modified_by << wu.recorded_at << fort::endr;
    }

    std::ostringstream ss;
    ss << std::endl << table.to_string() << std::endl;
    return ss.str();
}

}
