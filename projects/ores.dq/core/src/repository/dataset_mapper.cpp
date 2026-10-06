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
 * Template: cpp_domain_type_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.dq.core/repository/dataset_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq.api/domain/dataset.hpp"
#include "ores.dq.api/domain/dataset_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/dataset_entity.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string_view>
#include <vector>

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::dataset dataset_mapper::map(const dataset_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::dataset r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());

    r.code = v.code;

    r.catalog_name = v.catalog_name;
    r.subject_area_name = v.subject_area_name;
    r.domain_name = v.domain_name;
    r.coding_scheme_code = v.coding_scheme_code;
    r.origin_code = v.origin_code;
    r.nature_code = v.nature_code;
    r.treatment_code = v.treatment_code;
    r.methodology_id =
        v.methodology_id.has_value() ?
            std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.methodology_id)) :
            std::nullopt;
    r.name = v.name;
    r.description = v.description;
    r.source_system_id = v.source_system_id;
    r.business_context = v.business_context;
    r.upstream_derivation_id =
        v.upstream_derivation_id.has_value() ?
            std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.upstream_derivation_id)) :
            std::nullopt;
    r.lineage_depth = v.lineage_depth;
    r.as_of_date = timestamp_to_timepoint(std::string_view{v.as_of_date});
    r.ingestion_timestamp = timestamp_to_timepoint(std::string_view{v.ingestion_timestamp});
    r.license_info = v.license_info;
    r.artefact_type = v.artefact_type;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

dataset_entity dataset_mapper::map(const domain::dataset& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    dataset_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;

    r.code = v.code;

    r.catalog_name = v.catalog_name;
    r.subject_area_name = v.subject_area_name;
    r.domain_name = v.domain_name;
    r.coding_scheme_code = v.coding_scheme_code;
    r.origin_code = v.origin_code;
    r.nature_code = v.nature_code;
    r.treatment_code = v.treatment_code;
    r.methodology_id = v.methodology_id.has_value() ?
                           std::optional(boost::uuids::to_string(*v.methodology_id)) :
                           std::nullopt;
    r.name = v.name;
    r.description = v.description;
    r.source_system_id = v.source_system_id;
    r.business_context = v.business_context;
    r.upstream_derivation_id =
        v.upstream_derivation_id.has_value() ?
            std::optional(boost::uuids::to_string(*v.upstream_derivation_id)) :
            std::nullopt;
    r.lineage_depth = v.lineage_depth;
    r.as_of_date = ores::platform::time::datetime::to_iso8601_utc(v.as_of_date);
    r.ingestion_timestamp = ores::platform::time::datetime::to_iso8601_utc(v.ingestion_timestamp);
    r.license_info = v.license_info;
    r.artefact_type = v.artefact_type;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::dataset> dataset_mapper::map(const std::vector<dataset_entity>& v) {
    return map_vector<dataset_entity, domain::dataset>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<dataset_entity> dataset_mapper::map(const std::vector<domain::dataset>& v) {
    return map_vector<domain::dataset, dataset_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
