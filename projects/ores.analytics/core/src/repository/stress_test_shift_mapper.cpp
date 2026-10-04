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
#include "ores.analytics.core/repository/stress_test_shift_mapper.hpp"
#include "ores.analytics.api/domain/stress_test_shift_json_io.hpp" // IWYU pragma: keep.
#include "ores.database/repository/mapper_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::analytics::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::stress_test_shift stress_test_shift_mapper::map(const stress_test_shift_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::stress_test_shift r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.stress_test_scenario_id = boost::lexical_cast<boost::uuids::uuid>(v.stress_test_scenario_id);
    r.family = v.family;
    r.object_key = v.object_key;
    r.shift_type = v.shift_type;
    r.shifts = v.shifts;
    r.shift_tenors = v.shift_tenors;
    r.shift_expiries = v.shift_expiries;
    r.extras = v.extras;
    r.position = v.position;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

stress_test_shift_entity stress_test_shift_mapper::map(const domain::stress_test_shift& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    stress_test_shift_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.stress_test_scenario_id = boost::uuids::to_string(v.stress_test_scenario_id);
    r.family = v.family;
    r.object_key = v.object_key;
    r.shift_type = v.shift_type;
    r.shifts = v.shifts;
    r.shift_tenors = v.shift_tenors;
    r.shift_expiries = v.shift_expiries;
    r.extras = v.extras;
    r.position = v.position;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::stress_test_shift>
stress_test_shift_mapper::map(const std::vector<stress_test_shift_entity>& v) {
    return map_vector<stress_test_shift_entity, domain::stress_test_shift>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<stress_test_shift_entity>
stress_test_shift_mapper::map(const std::vector<domain::stress_test_shift>& v) {
    return map_vector<domain::stress_test_shift, stress_test_shift_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
