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
#include "ores.inbox.core/repository/approval_decision_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.inbox.api/domain/approval_decision_json_io.hpp" // IWYU pragma: keep.
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <sstream>

namespace ores::inbox::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::approval_decision approval_decision_mapper::map(const approval_decision_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::approval_decision r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.request_id = boost::lexical_cast<boost::uuids::uuid>(v.request_id);
    r.decision_code = v.decision_code;
    r.decided_by = boost::lexical_cast<boost::uuids::uuid>(v.decided_by);
    r.decided_at = timestamp_to_timepoint(std::string_view{v.decided_at});
    r.comment = v.comment;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

approval_decision_entity approval_decision_mapper::map(const domain::approval_decision& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    approval_decision_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.request_id = boost::uuids::to_string(v.request_id);
    r.decision_code = v.decision_code;
    r.decided_by = boost::uuids::to_string(v.decided_by);
    r.decided_at = ores::platform::time::datetime::to_iso8601_utc(v.decided_at);
    r.comment = v.comment;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::approval_decision>
approval_decision_mapper::map(const std::vector<approval_decision_entity>& v) {
    return map_vector<approval_decision_entity, domain::approval_decision>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<approval_decision_entity>
approval_decision_mapper::map(const std::vector<domain::approval_decision>& v) {
    return map_vector<domain::approval_decision, approval_decision_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
