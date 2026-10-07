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
#include "ores.marketdata.core/repository/observation_lineage_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/observation_lineage.hpp"
#include "ores.marketdata.api/domain/observation_lineage_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/observation_lineage_entity.hpp"
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

domain::observation_lineage observation_lineage_mapper::map(const observation_lineage_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::observation_lineage r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);

    r.series_id = boost::lexical_cast<boost::uuids::uuid>(v.series_id);

    r.observation_datetime = timestamp_to_timepoint(std::string_view{v.observation_datetime});


    r.oresmd_uri = v.oresmd_uri;

    r.point_source_kind = v.point_source_kind;
    r.derivation_config_id =
        v.derivation_config_id.has_value() ?
            std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.derivation_config_id)) :
            std::nullopt;
    r.derivation_config_version = v.derivation_config_version;
    r.source_as_of = v.source_as_of.has_value() ?
                         std::optional(timestamp_to_timepoint(*v.source_as_of)) :
                         std::nullopt;
    r.source_series_ids = v.source_series_ids;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

observation_lineage_entity observation_lineage_mapper::map(const domain::observation_lineage& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    observation_lineage_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);

    r.series_id = boost::uuids::to_string(v.series_id);

    r.observation_datetime = ores::platform::time::datetime::to_iso8601_utc(v.observation_datetime);


    r.oresmd_uri = v.oresmd_uri;

    r.point_source_kind = v.point_source_kind;
    r.derivation_config_id = v.derivation_config_id.has_value() ?
                                 std::optional(boost::uuids::to_string(*v.derivation_config_id)) :
                                 std::nullopt;
    r.derivation_config_version = v.derivation_config_version;
    r.source_as_of =
        v.source_as_of.has_value() ?
            std::optional(ores::platform::time::datetime::to_db_string(*v.source_as_of)) :
            std::nullopt;
    r.source_series_ids = v.source_series_ids;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::observation_lineage>
observation_lineage_mapper::map(const std::vector<observation_lineage_entity>& v) {
    return map_vector<observation_lineage_entity, domain::observation_lineage>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<observation_lineage_entity>
observation_lineage_mapper::map(const std::vector<domain::observation_lineage>& v) {
    return map_vector<domain::observation_lineage, observation_lineage_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
