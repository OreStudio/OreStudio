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
#include "ores.dq.core/presentation/lei_entity_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"

namespace ores::dq::presentation {

std::vector<ores::diff::domain::field_value> render_lei_entity_fields(const domain::lei_entity& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Lei", .value = v.lei});
    fields.push_back({.name = "Entity Legal Name", .value = v.entity_legal_name});
    fields.push_back({.name = "Entity Entity Category", .value = v.entity_entity_category});
    fields.push_back({.name = "Entity Entity Sub Category",
                      .value = v.entity_entity_sub_category.value_or(std::string{})});
    fields.push_back({.name = "Entity Entity Status", .value = v.entity_entity_status});
    fields.push_back({.name = "Entity Legal Form Entity Legal Form Code",
                      .value = v.entity_legal_form_entity_legal_form_code.value_or(std::string{})});
    fields.push_back({.name = "Entity Legal Form Other Legal Form",
                      .value = v.entity_legal_form_other_legal_form.value_or(std::string{})});
    fields.push_back({.name = "Entity Legal Jurisdiction",
                      .value = v.entity_legal_jurisdiction.value_or(std::string{})});
    fields.push_back({.name = "Entity Legal Address First Address Line",
                      .value = v.entity_legal_address_first_address_line.value_or(std::string{})});
    fields.push_back({.name = "Entity Legal Address City",
                      .value = v.entity_legal_address_city.value_or(std::string{})});
    fields.push_back({.name = "Entity Legal Address Region",
                      .value = v.entity_legal_address_region.value_or(std::string{})});
    fields.push_back(
        {.name = "Entity Legal Address Country", .value = v.entity_legal_address_country});
    fields.push_back({.name = "Entity Legal Address Postal Code",
                      .value = v.entity_legal_address_postal_code.value_or(std::string{})});
    fields.push_back(
        {.name = "Entity Headquarters Address First Address Line",
         .value = v.entity_headquarters_address_first_address_line.value_or(std::string{})});
    fields.push_back({.name = "Entity Headquarters Address City",
                      .value = v.entity_headquarters_address_city.value_or(std::string{})});
    fields.push_back({.name = "Entity Headquarters Address Region",
                      .value = v.entity_headquarters_address_region.value_or(std::string{})});
    fields.push_back({.name = "Entity Headquarters Address Country",
                      .value = v.entity_headquarters_address_country.value_or(std::string{})});
    fields.push_back({.name = "Entity Headquarters Address Postal Code",
                      .value = v.entity_headquarters_address_postal_code.value_or(std::string{})});
    fields.push_back({.name = "Entity Entity Creation Date",
                      .value = v.entity_entity_creation_date ?
                                   ores::platform::time::datetime::to_iso8601_utc(
                                       *v.entity_entity_creation_date) :
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
    fields.push_back({.name = "Registration Next Renewal Date",
                      .value = v.registration_next_renewal_date ?
                                   ores::platform::time::datetime::to_iso8601_utc(
                                       *v.registration_next_renewal_date) :
                                   std::string{}});
    fields.push_back({.name = "Registration Registration Status",
                      .value = v.registration_registration_status.value_or(std::string{})});
    fields.push_back({.name = "Entity Transliterated Name 1",
                      .value = v.entity_transliterated_name_1.value_or(std::string{})});
    fields.push_back({.name = "Entity Transliterated Name 1 Type",
                      .value = v.entity_transliterated_name_1_type.value_or(std::string{})});
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
