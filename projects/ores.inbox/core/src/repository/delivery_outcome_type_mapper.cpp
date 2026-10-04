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
#include "ores.inbox.core/repository/delivery_outcome_type_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.inbox.api/domain/delivery_outcome_type_json_io.hpp" // IWYU pragma: keep.

namespace ores::inbox::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::delivery_outcome_type
delivery_outcome_type_mapper::map(const delivery_outcome_type_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::delivery_outcome_type r;
    r.code = v.code.value();
    r.description = v.description;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

delivery_outcome_type_entity
delivery_outcome_type_mapper::map(const domain::delivery_outcome_type& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    delivery_outcome_type_entity r;
    r.code = v.code;
    r.description = v.description;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::delivery_outcome_type>
delivery_outcome_type_mapper::map(const std::vector<delivery_outcome_type_entity>& v) {
    return map_vector<delivery_outcome_type_entity, domain::delivery_outcome_type>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<delivery_outcome_type_entity>
delivery_outcome_type_mapper::map(const std::vector<domain::delivery_outcome_type>& v) {
    return map_vector<domain::delivery_outcome_type, delivery_outcome_type_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
