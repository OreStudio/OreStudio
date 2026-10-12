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
 * Template: cpp_domain_type_table.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.trading.api/domain/synthetic_cdo_instrument_table.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>

namespace ores::trading::domain {


std::string convert_to_table(const std::vector<synthetic_cdo_instrument>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "ID" << "Qualifier" << "Protection Start" << "Attachment"
          << "Detachment" << "Modified By" << "Version" << fort::endr;

    for ([[maybe_unused]] const auto& sc : v) {
        table << sc.trade_id << sc.qualifier << sc.protection_start_date << sc.attachment_point
              << sc.detachment_point << sc.modified_by << sc.version << fort::endr;
    }
    return table.to_string();
}

}
