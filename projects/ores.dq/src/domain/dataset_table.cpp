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
#include "ores.dq/domain/dataset_table.hpp"

#include <format>
#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>

namespace ores::dq::domain {

std::string convert_to_table(const std::vector<dataset>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "ID" << "Name" << "Subject Area"
          << "Domain" << "Origin" << "Nature" << "Treatment"
          << "Lineage Depth" << "As Of Date" << "Version" << fort::endr;

    for (const auto& d : v) {
        const auto as_of = std::format("{:%Y-%m-%d}", d.as_of_date);
        table << boost::uuids::to_string(d.id) << d.name << d.subject_area_name
              << d.domain_name << d.origin_code << d.nature_code << d.treatment_code
              << d.lineage_depth << as_of << d.version << fort::endr;
    }
    return table.to_string();
}

}
