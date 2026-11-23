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
#include "ores.iam.core/repository/agent_held_write_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/agent_held_write.hpp"
#include "ores.iam.api/domain/agent_held_write_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/agent_held_write_entity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <chrono>
#include <string_view>
#include <vector>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::agent_held_write agent_held_write_mapper::map(const agent_held_write_entity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::agent_held_write r;
    r.request_id = v.request_id;
    r.held_at = v.held_at;
    r.tenant_id = v.tenant_id;
    r.party_id = v.party_id;
    r.grant_id = v.grant_id;
    r.grantor_account_id = v.grantor_account_id;
    r.method = v.method;
    r.route = v.route;
    r.target = v.target;
    r.content_type = v.content_type;
    r.body = v.body;
    r.state = v.state;
    r.outcome = v.outcome;
    r.status = v.status;

    ORES_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

agent_held_write_entity agent_held_write_mapper::map(const domain::agent_held_write& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    agent_held_write_entity r;
    r.request_id = v.request_id;
    r.held_at = v.held_at;
    r.tenant_id = v.tenant_id;
    r.party_id = v.party_id;
    r.grant_id = v.grant_id;
    r.grantor_account_id = v.grantor_account_id;
    r.method = v.method;
    r.route = v.route;
    r.target = v.target;
    r.content_type = v.content_type;
    r.body = v.body;
    r.state = v.state;
    r.outcome = v.outcome;
    r.status = v.status;

    ORES_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::agent_held_write>
agent_held_write_mapper::map(const std::vector<agent_held_write_entity>& v) {
    return map_vector<agent_held_write_entity, domain::agent_held_write>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<agent_held_write_entity>
agent_held_write_mapper::map(const std::vector<domain::agent_held_write>& v) {
    return map_vector<domain::agent_held_write, agent_held_write_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
