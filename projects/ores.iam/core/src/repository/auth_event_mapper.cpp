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
#include "ores.iam.core/repository/auth_event_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/auth_event.hpp"
#include "ores.iam.api/domain/auth_event_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/auth_event_entity.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <chrono>
#include <string_view>
#include <vector>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::auth_event auth_event_mapper::map(const auth_event_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::auth_event r;
    r.id = v.id;
    r.event_time = v.event_time;
    r.tenant_id = v.tenant_id;
    r.account_id = v.account_id;
    r.event_type = v.event_type;
    r.username = v.username;
    r.session_id = v.session_id;
    r.party_id = v.party_id;
    r.error_detail = v.error_detail;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

auth_event_entity auth_event_mapper::map(const domain::auth_event& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    auth_event_entity r;
    r.id = v.id;
    r.event_time = v.event_time;
    r.tenant_id = v.tenant_id;
    r.account_id = v.account_id;
    r.event_type = v.event_type;
    r.username = v.username;
    r.session_id = v.session_id;
    r.party_id = v.party_id;
    r.error_detail = v.error_detail;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::auth_event> auth_event_mapper::map(const std::vector<auth_event_entity>& v) {
    return map_vector<auth_event_entity, domain::auth_event>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<auth_event_entity> auth_event_mapper::map(const std::vector<domain::auth_event>& v) {
    return map_vector<domain::auth_event, auth_event_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
