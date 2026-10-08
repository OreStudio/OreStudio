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
#include "ores.marketdata.core/repository/series_axis_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/series_axis.hpp"
#include "ores.marketdata.api/domain/series_axis_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/series_axis_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::series_axis series_axis_mapper::map(const series_axis_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::series_axis r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.series_id = boost::lexical_cast<boost::uuids::uuid>(v.series_id.value());
    r.axis_field = v.axis_field.value();
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.sequence = v.sequence;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

series_axis_entity series_axis_mapper::map(const domain::series_axis& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    series_axis_entity r;
    r.series_id = boost::uuids::to_string(v.series_id);
    r.axis_field = v.axis_field;
    r.tenant_id = v.tenant_id.to_string();
    r.party_id = boost::uuids::to_string(v.party_id);
    r.sequence = v.sequence;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::series_axis> series_axis_mapper::map(const std::vector<series_axis_entity>& v) {
    return map_vector<series_axis_entity, domain::series_axis>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<series_axis_entity> series_axis_mapper::map(const std::vector<domain::series_axis>& v) {
    return map_vector<domain::series_axis, series_axis_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
