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
#include "ores.scheduler.core/repository/retention_run_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.scheduler.api/domain/retention_run.hpp"
#include "ores.scheduler.api/domain/retention_run_json_io.hpp" // IWYU pragma: keep.
#include "ores.scheduler.core/repository/retention_run_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <string_view>
#include <vector>

namespace ores::scheduler::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::retention_run retention_run_mapper::map(const retention_run_entity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::retention_run r;
    r.id = v.id;
    r.policy_code = v.policy_code;
    r.target = v.target;
    r.action = v.action;
    r.retention_days = v.retention_days;
    r.enabled = v.enabled;
    r.ran_at = v.ran_at;
    r.removed = v.removed;
    r.outcome = v.outcome;
    r.detail = v.detail;

    ORES_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

retention_run_entity retention_run_mapper::map(const domain::retention_run& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    retention_run_entity r;
    r.id = v.id;
    r.policy_code = v.policy_code;
    r.target = v.target;
    r.action = v.action;
    r.retention_days = v.retention_days;
    r.enabled = v.enabled;
    r.ran_at = v.ran_at;
    r.removed = v.removed;
    r.outcome = v.outcome;
    r.detail = v.detail;

    ORES_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::retention_run>
retention_run_mapper::map(const std::vector<retention_run_entity>& v) {
    return map_vector<retention_run_entity, domain::retention_run>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<retention_run_entity>
retention_run_mapper::map(const std::vector<domain::retention_run>& v) {
    return map_vector<domain::retention_run, retention_run_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
