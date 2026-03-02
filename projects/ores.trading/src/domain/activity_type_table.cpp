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
#include "ores.trading/domain/activity_type_table.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>

namespace ores::trading::domain {

std::string convert_to_table(const std::vector<activity_type>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header
          << "version" << "code" << "category" << "requires_confirmation"
          << "description" << "fpml_event_type_code" << "fsm_transition_id"
          << "modified_by"
          << fort::endr;

    for (const auto& at : v) {
        table << at.version << at.code << at.category << at.requires_confirmation
              << at.description << at.fpml_event_type_code
              << (at.fsm_transition_id ? boost::uuids::to_string(*at.fsm_transition_id) : "")
              << at.modified_by
              << fort::endr;
    }
    return table.to_string();
}

}
