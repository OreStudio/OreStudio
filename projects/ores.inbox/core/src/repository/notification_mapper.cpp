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
#include "ores.inbox.core/repository/notification_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.inbox.api/domain/notification.hpp"
#include "ores.inbox.api/domain/notification_json_io.hpp" // IWYU pragma: keep.
#include "ores.inbox.core/repository/notification_entity.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <string_view>
#include <vector>

namespace ores::inbox::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::notification notification_mapper::map(const notification_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::notification r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.kind_code = v.kind_code;
    r.raised_by = boost::lexical_cast<boost::uuids::uuid>(v.raised_by);
    r.raised_at = timestamp_to_timepoint(std::string_view{v.raised_at});
    r.link_route = v.link_route;
    r.link_id = v.link_id;
    r.audience_permission_code = v.audience_permission_code;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

notification_entity notification_mapper::map(const domain::notification& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    notification_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.kind_code = v.kind_code;
    r.raised_by = boost::uuids::to_string(v.raised_by);
    r.raised_at = ores::platform::time::datetime::to_iso8601_utc(v.raised_at);
    r.link_route = v.link_route;
    r.link_id = v.link_id;
    r.audience_permission_code = v.audience_permission_code;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::notification>
notification_mapper::map(const std::vector<notification_entity>& v) {
    return map_vector<notification_entity, domain::notification>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<notification_entity>
notification_mapper::map(const std::vector<domain::notification>& v) {
    return map_vector<domain::notification, notification_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
