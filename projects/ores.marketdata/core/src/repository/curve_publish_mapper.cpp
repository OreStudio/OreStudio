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
#include "ores.marketdata.core/repository/curve_publish_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/curve_publish.hpp"
#include "ores.marketdata.api/domain/curve_publish_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/curve_publish_entity.hpp"
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

domain::curve_publish curve_publish_mapper::map(const curve_publish_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::curve_publish r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.request_id = v.request_id;
    r.party_id = v.party_id;
    r.bootstrap_config_id = v.bootstrap_config_id;
    r.output_series_id = v.output_series_id;
    r.as_of = v.as_of;
    r.published_by = v.published_by;
    r.published_at = v.published_at;
    r.withdrawn_by = v.withdrawn_by;
    r.withdrawn_at = v.withdrawn_at;
    r.withdrawn_reason_code = v.withdrawn_reason_code;
    r.withdrawal_request_id = v.withdrawal_request_id;
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

curve_publish_entity curve_publish_mapper::map(const domain::curve_publish& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    curve_publish_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.request_id = v.request_id;
    r.party_id = v.party_id;
    r.bootstrap_config_id = v.bootstrap_config_id;
    r.output_series_id = v.output_series_id;
    r.as_of = v.as_of;
    r.published_by = v.published_by;
    r.published_at = v.published_at;
    r.withdrawn_by = v.withdrawn_by;
    r.withdrawn_at = v.withdrawn_at;
    r.withdrawn_reason_code = v.withdrawn_reason_code;
    r.withdrawal_request_id = v.withdrawal_request_id;
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

std::vector<domain::curve_publish>
curve_publish_mapper::map(const std::vector<curve_publish_entity>& v) {
    return map_vector<curve_publish_entity, domain::curve_publish>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<curve_publish_entity>
curve_publish_mapper::map(const std::vector<domain::curve_publish>& v) {
    return map_vector<domain::curve_publish, curve_publish_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
