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
#include "ores.compute.api/domain/host_table.hpp"

#include <sstream>
#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>

namespace ores::compute::domain {

std::string convert_to_table(const std::vector<host>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header
          << "ID" << "External ID" << "Location" << "CPU Count"
          << "RAM (MB)" << "GPU Type" << "Last RPC Time" << "Credit Total"
          << "Modified By" << "Recorded At" << fort::endr;

    for (const auto& h : v) {
        table << boost::uuids::to_string(h.id)
              << h.external_id << h.location << h.cpu_count
              << h.ram_mb << h.gpu_type << h.last_rpc_time << h.credit_total
              << h.modified_by << h.recorded_at << fort::endr;
    }

    std::ostringstream ss;
    ss << std::endl << table.to_string() << std::endl;
    return ss.str();
}

}
