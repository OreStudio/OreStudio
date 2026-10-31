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
#include "ores.reporting.core/repository/cut_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/domain/cut.hpp"
#include "ores.reporting.api/domain/cut_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/cut_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string_view>
#include <vector>

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::cut cut_mapper::map(const cut_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::cut r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.kind_code = v.kind_code;
    r.party_id = v.party_id;
    r.scope_code = v.scope_code;
    r.business_centre_code = v.business_centre_code;
    r.timepoint = v.timepoint;
    r.take_ordinal = v.take_ordinal;
    r.timepoint_timezone = v.timepoint_timezone;
    r.cut_time_code = v.cut_time_code;
    r.market_data_snapshot_id = v.market_data_snapshot_id;
    r.deal_population_id = v.deal_population_id;
    r.reference_data_version = v.reference_data_version;
    r.engine_version = v.engine_version;
    r.code_version = v.code_version;
    r.taken_by = v.taken_by;
    r.taken_at = v.taken_at;
    r.requires_authorisation = v.requires_authorisation;
    r.approval_request_id = v.approval_request_id;
    r.approved_by = v.approved_by;
    r.approved_at = v.approved_at;
    r.refused_at = v.refused_at;
    r.refusal_comment = v.refusal_comment;
    r.retention_months = v.retention_months;
    r.retention_destination_code = v.retention_destination_code;
    r.legal_hold = v.legal_hold;
    r.output_is_indicative = v.output_is_indicative;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

cut_entity cut_mapper::map(const domain::cut& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    cut_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.kind_code = v.kind_code;
    r.party_id = v.party_id;
    r.scope_code = v.scope_code;
    r.business_centre_code = v.business_centre_code;
    r.timepoint = v.timepoint;
    r.take_ordinal = v.take_ordinal;
    r.timepoint_timezone = v.timepoint_timezone;
    r.cut_time_code = v.cut_time_code;
    r.market_data_snapshot_id = v.market_data_snapshot_id;
    r.deal_population_id = v.deal_population_id;
    r.reference_data_version = v.reference_data_version;
    r.engine_version = v.engine_version;
    r.code_version = v.code_version;
    r.taken_by = v.taken_by;
    r.taken_at = v.taken_at;
    r.requires_authorisation = v.requires_authorisation;
    r.approval_request_id = v.approval_request_id;
    r.approved_by = v.approved_by;
    r.approved_at = v.approved_at;
    r.refused_at = v.refused_at;
    r.refusal_comment = v.refusal_comment;
    r.retention_months = v.retention_months;
    r.retention_destination_code = v.retention_destination_code;
    r.legal_hold = v.legal_hold;
    r.output_is_indicative = v.output_is_indicative;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::cut> cut_mapper::map(const std::vector<cut_entity>& v) {
    return map_vector<cut_entity, domain::cut>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<cut_entity> cut_mapper::map(const std::vector<domain::cut>& v) {
    return map_vector<domain::cut, cut_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
