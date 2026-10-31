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
#include "ores.reporting.core/repository/cut_gap_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.reporting.api/domain/cut_gap.hpp"
#include "ores.reporting.api/domain/cut_gap_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/cut_gap_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::cut_gap cut_gap_mapper::map(const cut_gap_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::cut_gap r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.cut_id = v.cut_id;
    r.part_code = v.part_code;
    r.item = v.item;
    r.party_id = v.party_id;
    r.reason = v.reason;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

cut_gap_entity cut_gap_mapper::map(const domain::cut_gap& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    cut_gap_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.cut_id = v.cut_id;
    r.part_code = v.part_code;
    r.item = v.item;
    r.party_id = v.party_id;
    r.reason = v.reason;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::cut_gap> cut_gap_mapper::map(const std::vector<cut_gap_entity>& v) {
    return map_vector<cut_gap_entity, domain::cut_gap>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<cut_gap_entity> cut_gap_mapper::map(const std::vector<domain::cut_gap>& v) {
    return map_vector<domain::cut_gap, cut_gap_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
