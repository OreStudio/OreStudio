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
#include "ores.marketdata.core/repository/market_data_mapping_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/market_data_mapping.hpp"
#include "ores.marketdata.api/domain/market_data_mapping_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/market_data_mapping_entity.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string_view>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::market_data_mapping market_data_mapping_mapper::map(const market_data_mapping_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::market_data_mapping r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.vendor_instrument_id = boost::lexical_cast<boost::uuids::uuid>(v.vendor_instrument_id);

    r.effective_from = timestamp_to_timepoint(std::string_view{v.effective_from});

    r.oresmd_uri = v.oresmd_uri;
    r.effective_to = v.effective_to.has_value() ?
                         std::optional(timestamp_to_timepoint(*v.effective_to)) :
                         std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

market_data_mapping_entity market_data_mapping_mapper::map(const domain::market_data_mapping& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    market_data_mapping_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.vendor_instrument_id = boost::uuids::to_string(v.vendor_instrument_id);

    r.effective_from = ores::platform::time::datetime::to_iso8601_utc(v.effective_from);

    r.oresmd_uri = v.oresmd_uri;
    r.effective_to =
        v.effective_to.has_value() ?
            std::optional(ores::platform::time::datetime::to_db_string(*v.effective_to)) :
            std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::market_data_mapping>
market_data_mapping_mapper::map(const std::vector<market_data_mapping_entity>& v) {
    return map_vector<market_data_mapping_entity, domain::market_data_mapping>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<market_data_mapping_entity>
market_data_mapping_mapper::map(const std::vector<domain::market_data_mapping>& v) {
    return map_vector<domain::market_data_mapping, market_data_mapping_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
