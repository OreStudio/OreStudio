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
#include "ores.refdata.core/repository/bond_yield_convention_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/bond_yield_convention_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::bond_yield_convention
bond_yield_convention_mapper::map(const bond_yield_convention_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::bond_yield_convention r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.workspace_id = boost::lexical_cast<boost::uuids::uuid>(v.workspace_id);
    r.id = v.id.value();
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.compounding = v.compounding;
    r.frequency = v.frequency;
    r.price_type = v.price_type;
    r.accuracy = v.accuracy;
    r.max_evaluations = v.max_evaluations;
    r.guess = v.guess;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

bond_yield_convention_entity
bond_yield_convention_mapper::map(const domain::bond_yield_convention& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    bond_yield_convention_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id.to_string();
    r.workspace_id = boost::uuids::to_string(v.workspace_id);
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.compounding = v.compounding;
    r.frequency = v.frequency;
    r.price_type = v.price_type;
    r.accuracy = v.accuracy;
    r.max_evaluations = v.max_evaluations;
    r.guess = v.guess;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::bond_yield_convention>
bond_yield_convention_mapper::map(const std::vector<bond_yield_convention_entity>& v) {
    return map_vector<bond_yield_convention_entity, domain::bond_yield_convention>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<bond_yield_convention_entity>
bond_yield_convention_mapper::map(const std::vector<domain::bond_yield_convention>& v) {
    return map_vector<domain::bond_yield_convention, bond_yield_convention_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
