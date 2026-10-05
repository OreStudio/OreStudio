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
#include "ores.refdata.core/repository/average_ois_convention_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/average_ois_convention.hpp"
#include "ores.refdata.api/domain/average_ois_convention_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/average_ois_convention_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::average_ois_convention
average_ois_convention_mapper::map(const average_ois_convention_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::average_ois_convention r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.workspace_id = boost::lexical_cast<boost::uuids::uuid>(v.workspace_id);
    r.id = v.id.value();
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.spot_lag = v.spot_lag;
    r.fixed_tenor = v.fixed_tenor;
    r.fixed_day_count_fraction = v.fixed_day_count_fraction;
    r.fixed_calendar = v.fixed_calendar;
    r.fixed_convention = v.fixed_convention;
    r.fixed_payment_convention = v.fixed_payment_convention;
    r.fixed_frequency = v.fixed_frequency;
    r.index = v.index;
    r.on_tenor = v.on_tenor;
    r.rate_cutoff = v.rate_cutoff;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

average_ois_convention_entity
average_ois_convention_mapper::map(const domain::average_ois_convention& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    average_ois_convention_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id.to_string();
    r.workspace_id = boost::uuids::to_string(v.workspace_id);
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.spot_lag = v.spot_lag;
    r.fixed_tenor = v.fixed_tenor;
    r.fixed_day_count_fraction = v.fixed_day_count_fraction;
    r.fixed_calendar = v.fixed_calendar;
    r.fixed_convention = v.fixed_convention;
    r.fixed_payment_convention = v.fixed_payment_convention;
    r.fixed_frequency = v.fixed_frequency;
    r.index = v.index;
    r.on_tenor = v.on_tenor;
    r.rate_cutoff = v.rate_cutoff;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::average_ois_convention>
average_ois_convention_mapper::map(const std::vector<average_ois_convention_entity>& v) {
    return map_vector<average_ois_convention_entity, domain::average_ois_convention>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<average_ois_convention_entity>
average_ois_convention_mapper::map(const std::vector<domain::average_ois_convention>& v) {
    return map_vector<domain::average_ois_convention, average_ois_convention_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
