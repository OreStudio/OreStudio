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
#include "ores.dq.core/presentation/lei_relationship_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"

namespace ores::dq::presentation {

std::vector<ores::diff::domain::field_value>
render_lei_relationship_fields(const domain::lei_relationship& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back(
        {.name = "Relationship Start Node Node ID", .value = v.relationship_start_node_node_id});
    fields.push_back({.name = "Relationship Start Node Node ID Type",
                      .value = v.relationship_start_node_node_id_type});
    fields.push_back(
        {.name = "Relationship End Node Node ID", .value = v.relationship_end_node_node_id});
    fields.push_back({.name = "Relationship End Node Node ID Type",
                      .value = v.relationship_end_node_node_id_type});
    fields.push_back(
        {.name = "Relationship Relationship Type", .value = v.relationship_relationship_type});
    fields.push_back(
        {.name = "Relationship Relationship Status", .value = v.relationship_relationship_status});
    fields.push_back({.name = "Relationship Period 1 Start Date",
                      .value = v.relationship_period_1_start_date ?
                                   ores::platform::time::datetime::to_iso8601_utc(
                                       *v.relationship_period_1_start_date) :
                                   std::string{}});
    fields.push_back({.name = "Relationship Period 1 End Date",
                      .value = v.relationship_period_1_end_date ?
                                   ores::platform::time::datetime::to_iso8601_utc(
                                       *v.relationship_period_1_end_date) :
                                   std::string{}});
    fields.push_back({.name = "Registration Initial Registration Date",
                      .value = v.registration_initial_registration_date ?
                                   ores::platform::time::datetime::to_iso8601_utc(
                                       *v.registration_initial_registration_date) :
                                   std::string{}});
    fields.push_back({.name = "Registration Last Update Date",
                      .value = v.registration_last_update_date ?
                                   ores::platform::time::datetime::to_iso8601_utc(
                                       *v.registration_last_update_date) :
                                   std::string{}});
    fields.push_back({.name = "Registration Registration Status",
                      .value = v.registration_registration_status.value_or(std::string{})});
    fields.push_back({.name = "Registration Validation Sources",
                      .value = v.registration_validation_sources.value_or(std::string{})});
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
