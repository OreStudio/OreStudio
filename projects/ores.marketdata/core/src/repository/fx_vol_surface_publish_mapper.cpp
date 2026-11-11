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
#include "ores.marketdata.core/repository/fx_vol_surface_publish_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/fx_vol_surface_publish.hpp"
#include "ores.marketdata.api/domain/fx_vol_surface_publish_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/fx_vol_surface_publish_entity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string_view>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::fx_vol_surface_publish
fx_vol_surface_publish_mapper::map(const fx_vol_surface_publish_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::fx_vol_surface_publish r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.request_id = v.request_id;
    r.party_id = v.party_id;
    r.blueprint_id = v.blueprint_id;
    r.as_of = v.as_of;
    r.output_series_id = v.output_series_id;
    r.calibration_id = v.calibration_id;
    r.calibration_version = v.calibration_version;
    r.converged = v.converged;
    r.max_abs_error = v.max_abs_error;
    r.pillars_beyond_tolerance = v.pillars_beyond_tolerance;
    r.violation_count = v.violation_count;
    r.published_by = v.published_by;
    r.published_at = v.published_at;
    r.superseded_at = v.superseded_at;
    r.superseded_by_request_id = v.superseded_by_request_id;
    r.superseded_as_of = v.superseded_as_of;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

fx_vol_surface_publish_entity
fx_vol_surface_publish_mapper::map(const domain::fx_vol_surface_publish& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    fx_vol_surface_publish_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.request_id = v.request_id;
    r.party_id = v.party_id;
    r.blueprint_id = v.blueprint_id;
    r.as_of = v.as_of;
    r.output_series_id = v.output_series_id;
    r.calibration_id = v.calibration_id;
    r.calibration_version = v.calibration_version;
    r.converged = v.converged;
    r.max_abs_error = v.max_abs_error;
    r.pillars_beyond_tolerance = v.pillars_beyond_tolerance;
    r.violation_count = v.violation_count;
    r.published_by = v.published_by;
    r.published_at = v.published_at;
    r.superseded_at = v.superseded_at;
    r.superseded_by_request_id = v.superseded_by_request_id;
    r.superseded_as_of = v.superseded_as_of;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::fx_vol_surface_publish>
fx_vol_surface_publish_mapper::map(const std::vector<fx_vol_surface_publish_entity>& v) {
    return map_vector<fx_vol_surface_publish_entity, domain::fx_vol_surface_publish>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<fx_vol_surface_publish_entity>
fx_vol_surface_publish_mapper::map(const std::vector<domain::fx_vol_surface_publish>& v) {
    return map_vector<domain::fx_vol_surface_publish, fx_vol_surface_publish_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
