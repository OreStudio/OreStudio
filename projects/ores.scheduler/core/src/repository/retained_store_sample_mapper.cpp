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
#include "ores.scheduler.core/repository/retained_store_sample_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.scheduler.api/domain/retained_store_sample.hpp"
#include "ores.scheduler.api/domain/retained_store_sample_json_io.hpp" // IWYU pragma: keep.
#include "ores.scheduler.core/repository/retained_store_sample_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <string_view>
#include <vector>

namespace ores::scheduler::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::retained_store_sample
retained_store_sample_mapper::map(const retained_store_sample_entity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::retained_store_sample r;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.sampled_at = v.sampled_at;
    r.store_code = v.store_code;
    r.size_bytes = v.size_bytes;
    r.measure_count = v.measure_count;

    ORES_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

retained_store_sample_entity
retained_store_sample_mapper::map(const domain::retained_store_sample& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    retained_store_sample_entity r;
    r.id = v.id;
    r.sampled_at = v.sampled_at;
    r.tenant_id = v.tenant_id;
    r.store_code = v.store_code;
    r.size_bytes = v.size_bytes;
    r.measure_count = v.measure_count;

    ORES_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::retained_store_sample>
retained_store_sample_mapper::map(const std::vector<retained_store_sample_entity>& v) {
    return map_vector<retained_store_sample_entity, domain::retained_store_sample>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<retained_store_sample_entity>
retained_store_sample_mapper::map(const std::vector<domain::retained_store_sample>& v) {
    return map_vector<domain::retained_store_sample, retained_store_sample_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
