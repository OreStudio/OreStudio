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
#include "ores.dq.core/presentation/change_reason_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"

namespace ores::dq::presentation {

std::vector<ores::diff::domain::field_value>
render_change_reason_fields(const domain::change_reason& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Code", .value = v.code});
    fields.push_back({.name = "Description", .value = v.description});
    fields.push_back({.name = "Category Code", .value = v.category_code});
    fields.push_back({.name = "Applies To New", .value = v.applies_to_new ? "true" : "false"});
    fields.push_back({.name = "Applies To Amend", .value = v.applies_to_amend ? "true" : "false"});
    fields.push_back(
        {.name = "Applies To Delete", .value = v.applies_to_delete ? "true" : "false"});
    fields.push_back(
        {.name = "Requires Commentary", .value = v.requires_commentary ? "true" : "false"});
    fields.push_back({.name = "Display Order", .value = std::to_string(v.display_order)});
    using ores::history::domain::provenance_fields;
    fields.push_back({.name = provenance_fields::modified_by, .value = v.modified_by});
    fields.push_back({.name = provenance_fields::performed_by, .value = v.performed_by});
    fields.push_back(
        {.name = provenance_fields::change_reason_code, .value = v.change_reason_code});
    fields.push_back({.name = provenance_fields::change_commentary, .value = v.change_commentary});
    fields.push_back({.name = provenance_fields::recorded_at,
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.recorded_at)});

    return fields;
}

}
