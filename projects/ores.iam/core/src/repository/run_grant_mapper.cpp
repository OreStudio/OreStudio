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
#include "ores.iam.core/repository/run_grant_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/run_grant.hpp"
#include "ores.iam.api/domain/run_grant_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/run_grant_entity.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <vector>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::run_grant run_grant_mapper::map(const run_grant_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::run_grant r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);


    r.resource = v.resource;

    r.grantor_account_id = boost::lexical_cast<boost::uuids::uuid>(v.grantor_account_id);

    r.role_id = boost::lexical_cast<boost::uuids::uuid>(v.role_id);
    r.audience = v.audience;
    r.max_runs = v.max_runs.value_or(0);
    if (v.not_after)
        r.not_after = timestamp_to_timepoint(*v.not_after);
    else
        r.not_after = {};
    if (v.revoked_at)
        r.revoked_at = timestamp_to_timepoint(*v.revoked_at);
    else
        r.revoked_at = {};
    r.revoked_by = v.revoked_by.value_or("");
    r.revoke_reason = v.revoke_reason.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

run_grant_entity run_grant_mapper::map(const domain::run_grant& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    run_grant_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);


    r.resource = v.resource;

    r.grantor_account_id = boost::uuids::to_string(v.grantor_account_id);

    r.role_id = boost::uuids::to_string(v.role_id);
    r.audience = v.audience;
    r.max_runs = v.max_runs == 0 ? std::nullopt : std::optional(v.max_runs);
    r.not_after = v.not_after != std::chrono::system_clock::time_point{} ?
                      std::optional(ores::platform::time::datetime::to_db_string(v.not_after)) :
                      std::nullopt;
    r.revoked_at = v.revoked_at != std::chrono::system_clock::time_point{} ?
                       std::optional(ores::platform::time::datetime::to_db_string(v.revoked_at)) :
                       std::nullopt;
    r.revoked_by = v.revoked_by.empty() ? std::nullopt : std::optional(v.revoked_by);
    r.revoke_reason = v.revoke_reason.empty() ? std::nullopt : std::optional(v.revoke_reason);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::run_grant> run_grant_mapper::map(const std::vector<run_grant_entity>& v) {
    return map_vector<run_grant_entity, domain::run_grant>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<run_grant_entity> run_grant_mapper::map(const std::vector<domain::run_grant>& v) {
    return map_vector<domain::run_grant, run_grant_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
