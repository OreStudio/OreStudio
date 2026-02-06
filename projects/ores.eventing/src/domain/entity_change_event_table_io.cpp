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
#include "ores.eventing/domain/entity_change_event_table_io.hpp"

#include <fort.hpp>
#include "ores.platform/time/datetime.hpp"

namespace ores::eventing::domain {

std::ostream& operator<<(std::ostream& s, const std::vector<entity_change_event>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header
          << "Entity" << "Timestamp" << "Tenant ID" << fort::endr;

    for (const auto& item : v) {
        using platform::time::datetime;
        auto timestamp_str = datetime::format_time_point(item.timestamp,
                "%Y-%m-%d %H:%M:%S");

        table << item.entity << timestamp_str << item.tenant_id << fort::endr;
    }

    return s << table.to_string();
}

}
