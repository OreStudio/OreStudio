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
#include "ores.marketdata.core/repository/market_state_payload_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/market_state_payload.hpp"
#include "ores.marketdata.api/domain/market_state_payload_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/market_state_payload_entity.hpp"
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

domain::market_state_payload
market_state_payload_mapper::map(const market_state_payload_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::market_state_payload r;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.party_id = v.party_id;
    r.report_instance_id = v.report_instance_id;
    r.family = v.family;
    r.file_name = v.file_name;
    r.as_of = v.as_of;
    r.report_definition_id = v.report_definition_id;
    r.row_count = v.row_count;
    r.column_count = v.column_count;
    r.column_names = v.column_names;
    r.key_values = v.key_values;
    r.keys_truncated = v.keys_truncated;
    r.payload = v.payload;
    r.payload_bytes = v.payload_bytes;
    r.content_sha256 = v.content_sha256;
    r.source = v.source;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

market_state_payload_entity
market_state_payload_mapper::map(const domain::market_state_payload& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    market_state_payload_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.party_id = v.party_id;
    r.report_instance_id = v.report_instance_id;
    r.family = v.family;
    r.file_name = v.file_name;
    r.as_of = v.as_of;
    r.report_definition_id = v.report_definition_id;
    r.row_count = v.row_count;
    r.column_count = v.column_count;
    r.column_names = v.column_names;
    r.key_values = v.key_values;
    r.keys_truncated = v.keys_truncated;
    r.payload = v.payload;
    r.payload_bytes = v.payload_bytes;
    r.content_sha256 = v.content_sha256;
    r.source = v.source;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::market_state_payload>
market_state_payload_mapper::map(const std::vector<market_state_payload_entity>& v) {
    return map_vector<market_state_payload_entity, domain::market_state_payload>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<market_state_payload_entity>
market_state_payload_mapper::map(const std::vector<domain::market_state_payload>& v) {
    return map_vector<domain::market_state_payload, market_state_payload_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
