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
#include "ores.synthetic.core/repository/variance_calibration_input_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.synthetic.api/domain/variance_calibration_input.hpp"
#include "ores.synthetic.api/domain/variance_calibration_input_json_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.core/repository/variance_calibration_input_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <sstream>
#include <vector>

namespace ores::synthetic::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::variance_calibration_input
variance_calibration_input_mapper::map(const variance_calibration_input_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::variance_calibration_input r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.party_id = v.party_id;
    r.base_currency_code = v.base_currency_code;
    r.quote_currency_code = v.quote_currency_code;
    r.fixing_uri = v.fixing_uri;
    r.window_end = v.window_end;
    r.window_days = v.window_days;
    r.provider_code = v.provider_code;
    r.inverted = v.inverted;
    r.policy_id = v.policy_id;
    r.policy_version = v.policy_version;
    r.point_count = v.point_count;
    r.used_count = v.used_count;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

variance_calibration_input_entity
variance_calibration_input_mapper::map(const domain::variance_calibration_input& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    variance_calibration_input_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.party_id = v.party_id;
    r.base_currency_code = v.base_currency_code;
    r.quote_currency_code = v.quote_currency_code;
    r.fixing_uri = v.fixing_uri;
    r.window_end = v.window_end;
    r.window_days = v.window_days;
    r.provider_code = v.provider_code;
    r.inverted = v.inverted;
    r.policy_id = v.policy_id;
    r.policy_version = v.policy_version;
    r.point_count = v.point_count;
    r.used_count = v.used_count;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::variance_calibration_input>
variance_calibration_input_mapper::map(const std::vector<variance_calibration_input_entity>& v) {
    return map_vector<variance_calibration_input_entity, domain::variance_calibration_input>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<variance_calibration_input_entity>
variance_calibration_input_mapper::map(const std::vector<domain::variance_calibration_input>& v) {
    return map_vector<domain::variance_calibration_input, variance_calibration_input_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
