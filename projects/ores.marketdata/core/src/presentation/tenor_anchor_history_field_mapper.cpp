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
#include "ores.marketdata.core/presentation/tenor_anchor_history_field_mapper.hpp"
#include "ores.platform/time/datetime.hpp"

namespace ores::marketdata::presentation {

std::vector<ores::diff::domain::field_value>
render_tenor_anchor_fields(const domain::tenor_anchor& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Code", .value = v.code});
    fields.push_back({.name = "Description", .value = v.description});
    fields.push_back({.name = "Modified By", .value = v.modified_by});
    fields.push_back({.name = "Performed By", .value = v.performed_by});
    fields.push_back({.name = "Change Reason Code", .value = v.change_reason_code});
    fields.push_back({.name = "Change Commentary", .value = v.change_commentary});
    fields.push_back({.name = "Recorded At",
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.recorded_at)});

    return fields;
}

}
