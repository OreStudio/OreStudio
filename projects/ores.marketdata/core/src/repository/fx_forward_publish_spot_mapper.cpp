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
#include "ores.marketdata.core/repository/fx_forward_publish_spot_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/fx_forward_publish_spot.hpp"
#include "ores.marketdata.api/domain/fx_forward_publish_spot_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/fx_forward_publish_spot_entity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <string_view>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::fx_forward_publish_spot
fx_forward_publish_spot_mapper::map(const fx_forward_publish_spot_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::fx_forward_publish_spot r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.publish_id = v.publish_id;
    r.leg_code = v.leg_code;
    r.pair_code = v.pair_code;
    r.series_id = v.series_id;
    r.observed_at = v.observed_at;
    r.value = v.value;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

fx_forward_publish_spot_entity
fx_forward_publish_spot_mapper::map(const domain::fx_forward_publish_spot& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    fx_forward_publish_spot_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.publish_id = v.publish_id;
    r.leg_code = v.leg_code;
    r.pair_code = v.pair_code;
    r.series_id = v.series_id;
    r.observed_at = v.observed_at;
    r.value = v.value;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::fx_forward_publish_spot>
fx_forward_publish_spot_mapper::map(const std::vector<fx_forward_publish_spot_entity>& v) {
    return map_vector<fx_forward_publish_spot_entity, domain::fx_forward_publish_spot>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<fx_forward_publish_spot_entity>
fx_forward_publish_spot_mapper::map(const std::vector<domain::fx_forward_publish_spot>& v) {
    return map_vector<domain::fx_forward_publish_spot, fx_forward_publish_spot_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
