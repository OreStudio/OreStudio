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
#include "ores.trading.core/repository/bond_issue_conversion_target_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/bond_issue_conversion_target_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::bond_issue_conversion_target
bond_issue_conversion_target_mapper::map(const bond_issue_conversion_target_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::bond_issue_conversion_target r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.issue_id = boost::lexical_cast<boost::uuids::uuid>(v.issue_id.value());
    r.sequence_number = boost::lexical_cast<int>(v.sequence_number.value());
    r.underlying_id = v.underlying_id;
    r.conversion_ratio = v.conversion_ratio;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

bond_issue_conversion_target_entity
bond_issue_conversion_target_mapper::map(const domain::bond_issue_conversion_target& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    bond_issue_conversion_target_entity r;
    r.issue_id = boost::uuids::to_string(v.issue_id);
    r.sequence_number = std::to_string(v.sequence_number);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.underlying_id = v.underlying_id;
    r.conversion_ratio = v.conversion_ratio;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::bond_issue_conversion_target> bond_issue_conversion_target_mapper::map(
    const std::vector<bond_issue_conversion_target_entity>& v) {
    return map_vector<bond_issue_conversion_target_entity, domain::bond_issue_conversion_target>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<bond_issue_conversion_target_entity> bond_issue_conversion_target_mapper::map(
    const std::vector<domain::bond_issue_conversion_target>& v) {
    return map_vector<domain::bond_issue_conversion_target, bond_issue_conversion_target_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
