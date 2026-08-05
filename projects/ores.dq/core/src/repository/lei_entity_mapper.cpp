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
#include "ores.dq.core/repository/lei_entity_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq.api/domain/lei_entity_json_io.hpp" // IWYU pragma: keep.

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::lei_entity lei_entity_mapper::map(const lei_entity_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::lei_entity r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.lei = v.lei.value();
    r.entity_legal_name = v.entity_legal_name;
    r.entity_entity_category = v.entity_entity_category;
    r.entity_entity_sub_category = v.entity_entity_sub_category;
    r.entity_entity_status = v.entity_entity_status;
    r.entity_legal_form_entity_legal_form_code = v.entity_legal_form_entity_legal_form_code;
    r.entity_legal_form_other_legal_form = v.entity_legal_form_other_legal_form;
    r.entity_legal_jurisdiction = v.entity_legal_jurisdiction;
    r.entity_legal_address_first_address_line = v.entity_legal_address_first_address_line;
    r.entity_legal_address_city = v.entity_legal_address_city;
    r.entity_legal_address_region = v.entity_legal_address_region;
    r.entity_legal_address_country = v.entity_legal_address_country;
    r.entity_legal_address_postal_code = v.entity_legal_address_postal_code;
    r.entity_headquarters_address_first_address_line =
        v.entity_headquarters_address_first_address_line;
    r.entity_headquarters_address_city = v.entity_headquarters_address_city;
    r.entity_headquarters_address_region = v.entity_headquarters_address_region;
    r.entity_headquarters_address_country = v.entity_headquarters_address_country;
    r.entity_headquarters_address_postal_code = v.entity_headquarters_address_postal_code;
    r.entity_entity_creation_date =
        v.entity_entity_creation_date.has_value() ?
            std::optional(timestamp_to_timepoint(*v.entity_entity_creation_date)) :
            std::nullopt;
    r.registration_initial_registration_date =
        v.registration_initial_registration_date.has_value() ?
            std::optional(timestamp_to_timepoint(*v.registration_initial_registration_date)) :
            std::nullopt;
    r.registration_last_update_date =
        v.registration_last_update_date.has_value() ?
            std::optional(timestamp_to_timepoint(*v.registration_last_update_date)) :
            std::nullopt;
    r.registration_next_renewal_date =
        v.registration_next_renewal_date.has_value() ?
            std::optional(timestamp_to_timepoint(*v.registration_next_renewal_date)) :
            std::nullopt;
    r.registration_registration_status = v.registration_registration_status;
    r.entity_transliterated_name_1 = v.entity_transliterated_name_1;
    r.entity_transliterated_name_1_type = v.entity_transliterated_name_1_type;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

lei_entity_entity lei_entity_mapper::map(const domain::lei_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    lei_entity_entity r;
    r.lei = v.lei;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.entity_legal_name = v.entity_legal_name;
    r.entity_entity_category = v.entity_entity_category;
    r.entity_entity_sub_category = v.entity_entity_sub_category;
    r.entity_entity_status = v.entity_entity_status;
    r.entity_legal_form_entity_legal_form_code = v.entity_legal_form_entity_legal_form_code;
    r.entity_legal_form_other_legal_form = v.entity_legal_form_other_legal_form;
    r.entity_legal_jurisdiction = v.entity_legal_jurisdiction;
    r.entity_legal_address_first_address_line = v.entity_legal_address_first_address_line;
    r.entity_legal_address_city = v.entity_legal_address_city;
    r.entity_legal_address_region = v.entity_legal_address_region;
    r.entity_legal_address_country = v.entity_legal_address_country;
    r.entity_legal_address_postal_code = v.entity_legal_address_postal_code;
    r.entity_headquarters_address_first_address_line =
        v.entity_headquarters_address_first_address_line;
    r.entity_headquarters_address_city = v.entity_headquarters_address_city;
    r.entity_headquarters_address_region = v.entity_headquarters_address_region;
    r.entity_headquarters_address_country = v.entity_headquarters_address_country;
    r.entity_headquarters_address_postal_code = v.entity_headquarters_address_postal_code;
    r.entity_entity_creation_date =
        v.entity_entity_creation_date.has_value() ?
            std::optional(timepoint_to_timestamp(*v.entity_entity_creation_date, lg())) :
            std::nullopt;
    r.registration_initial_registration_date =
        v.registration_initial_registration_date.has_value() ?
            std::optional(timepoint_to_timestamp(*v.registration_initial_registration_date, lg())) :
            std::nullopt;
    r.registration_last_update_date =
        v.registration_last_update_date.has_value() ?
            std::optional(timepoint_to_timestamp(*v.registration_last_update_date, lg())) :
            std::nullopt;
    r.registration_next_renewal_date =
        v.registration_next_renewal_date.has_value() ?
            std::optional(timepoint_to_timestamp(*v.registration_next_renewal_date, lg())) :
            std::nullopt;
    r.registration_registration_status = v.registration_registration_status;
    r.entity_transliterated_name_1 = v.entity_transliterated_name_1;
    r.entity_transliterated_name_1_type = v.entity_transliterated_name_1_type;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::lei_entity> lei_entity_mapper::map(const std::vector<lei_entity_entity>& v) {
    return map_vector<lei_entity_entity, domain::lei_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<lei_entity_entity> lei_entity_mapper::map(const std::vector<domain::lei_entity>& v) {
    return map_vector<domain::lei_entity, lei_entity_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
