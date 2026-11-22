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
#include "ores.iam.core/repository/agent_grant_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/agent_grant.hpp"
#include "ores.iam.api/domain/agent_grant_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/agent_grant_entity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string_view>
#include <vector>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::agent_grant agent_grant_mapper::map(const agent_grant_entity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::agent_grant r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.party_id = v.party_id;
    r.grantor_account_id = v.grantor_account_id;
    r.agent_name = v.agent_name;
    r.role_id = v.role_id;
    r.not_after = v.not_after;
    r.revoked_at = v.revoked_at.value_or(db_timestamp{});
    r.revoked_by = v.revoked_by.value_or("");
    r.revoke_reason = v.revoke_reason.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    ORES_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

agent_grant_entity agent_grant_mapper::map(const domain::agent_grant& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    agent_grant_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.party_id = v.party_id;
    r.grantor_account_id = v.grantor_account_id;
    r.agent_name = v.agent_name;
    r.role_id = v.role_id;
    r.not_after = v.not_after;
    r.revoked_at = v.revoked_at == db_timestamp{} ? std::nullopt : std::optional(v.revoked_at);
    r.revoked_by = v.revoked_by.empty() ? std::nullopt : std::optional(v.revoked_by);
    r.revoke_reason = v.revoke_reason.empty() ? std::nullopt : std::optional(v.revoke_reason);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    ORES_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::agent_grant> agent_grant_mapper::map(const std::vector<agent_grant_entity>& v) {
    return map_vector<agent_grant_entity, domain::agent_grant>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<agent_grant_entity> agent_grant_mapper::map(const std::vector<domain::agent_grant>& v) {
    return map_vector<domain::agent_grant, agent_grant_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
