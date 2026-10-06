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
#include "ores.dq.core/presentation/dataset_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.dq.api/domain/dataset.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::dq::presentation {

std::vector<ores::diff::domain::field_value> render_dataset_fields(const domain::dataset& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Code", .value = v.code});
    fields.push_back({.name = "Catalog Name", .value = v.catalog_name.value_or(std::string{})});
    fields.push_back({.name = "Subject Area Name", .value = v.subject_area_name});
    fields.push_back({.name = "Domain Name", .value = v.domain_name});
    fields.push_back(
        {.name = "Coding Scheme Code", .value = v.coding_scheme_code.value_or(std::string{})});
    fields.push_back({.name = "Origin Code", .value = v.origin_code});
    fields.push_back({.name = "Nature Code", .value = v.nature_code});
    fields.push_back({.name = "Treatment Code", .value = v.treatment_code});
    fields.push_back(
        {.name = "Methodology ID",
         .value = v.methodology_id ? boost::uuids::to_string(*v.methodology_id) : std::string{}});
    fields.push_back({.name = "Name", .value = v.name});
    fields.push_back({.name = "Description", .value = v.description});
    fields.push_back({.name = "Source System ID", .value = v.source_system_id});
    fields.push_back({.name = "Business Context", .value = v.business_context});
    fields.push_back({.name = "Upstream Derivation ID",
                      .value = v.upstream_derivation_id ?
                                   boost::uuids::to_string(*v.upstream_derivation_id) :
                                   std::string{}});
    fields.push_back({.name = "Lineage Depth", .value = std::to_string(v.lineage_depth)});
    fields.push_back({.name = "As Of Date",
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.as_of_date)});
    fields.push_back(
        {.name = "Ingestion Timestamp",
         .value = ores::platform::time::datetime::to_iso8601_utc(v.ingestion_timestamp)});
    fields.push_back({.name = "License Info", .value = v.license_info.value_or(std::string{})});
    fields.push_back({.name = "Artefact Type", .value = v.artefact_type});
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
