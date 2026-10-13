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
#include "ores.iam.core/repository/session_sample_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/session_sample.hpp"
#include "ores.iam.api/domain/session_sample_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/session_sample_entity.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <string_view>
#include <vector>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::session_sample session_sample_mapper::map(const session_sample_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::session_sample r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.session_id = boost::lexical_cast<boost::uuids::uuid>(v.session_id.value());
    r.sample_time = timestamp_to_timepoint(std::string_view{v.sample_time.value()});
    r.bytes_sent = v.bytes_sent;
    r.bytes_received = v.bytes_received;
    r.latency_ms = v.latency_ms;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

session_sample_entity session_sample_mapper::map(const domain::session_sample& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    session_sample_entity r;
    r.session_id = boost::uuids::to_string(v.session_id);
    r.sample_time = ores::platform::time::datetime::to_db_string(v.sample_time);
    r.tenant_id = v.tenant_id.to_string();
    r.bytes_sent = v.bytes_sent;
    r.bytes_received = v.bytes_received;
    r.latency_ms = v.latency_ms;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::session_sample>
session_sample_mapper::map(const std::vector<session_sample_entity>& v) {
    return map_vector<session_sample_entity, domain::session_sample>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<session_sample_entity>
session_sample_mapper::map(const std::vector<domain::session_sample>& v) {
    return map_vector<domain::session_sample, session_sample_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
