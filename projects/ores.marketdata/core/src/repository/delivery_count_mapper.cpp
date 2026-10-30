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
#include "ores.marketdata.core/repository/delivery_count_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/delivery_count.hpp"
#include "ores.marketdata.api/domain/delivery_count_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/delivery_count_entity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <string_view>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::delivery_count delivery_count_mapper::map(const delivery_count_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::delivery_count r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.party_id = v.party_id;
    r.provider_code = v.provider_code;
    r.oresmd_uri = v.oresmd_uri;
    r.period_start = v.period_start;
    r.flushed_at = v.flushed_at;
    r.feed_id = v.feed_id;
    r.deliveries = v.deliveries;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

delivery_count_entity delivery_count_mapper::map(const domain::delivery_count& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    delivery_count_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.party_id = v.party_id;
    r.provider_code = v.provider_code;
    r.oresmd_uri = v.oresmd_uri;
    r.period_start = v.period_start;
    r.flushed_at = v.flushed_at;
    r.feed_id = v.feed_id;
    r.deliveries = v.deliveries;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::delivery_count>
delivery_count_mapper::map(const std::vector<delivery_count_entity>& v) {
    return map_vector<delivery_count_entity, domain::delivery_count>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<delivery_count_entity>
delivery_count_mapper::map(const std::vector<domain::delivery_count>& v) {
    return map_vector<domain::delivery_count, delivery_count_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
