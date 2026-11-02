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
 * Template: cpp_history_field_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.reporting.core/presentation/ore_file_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/domain/ore_file.hpp"
#include <string>
#include <vector>

namespace ores::reporting::presentation {

std::vector<ores::diff::domain::field_value> render_ore_file_fields(const domain::ore_file& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Code", .value = v.code});
    fields.push_back({.name = "Name", .value = v.name});
    fields.push_back({.name = "Category Code", .value = v.category_code});
    fields.push_back({.name = "Configuration Type Code",
                      .value = v.configuration_type_code.value_or(std::string{})});
    fields.push_back({.name = "Guide Section", .value = v.guide_section.value_or(std::string{})});
    fields.push_back({.name = "Origin", .value = v.origin});
    fields.push_back({.name = "Format", .value = v.format});
    fields.push_back(
        {.name = "Schema Ore File Code", .value = v.schema_ore_file_code.value_or(std::string{})});
    fields.push_back({.name = "Typical Filename", .value = v.typical_filename});
    fields.push_back({.name = "Filename Regex", .value = v.filename_regex});
    fields.push_back({.name = "Description", .value = v.description});
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
