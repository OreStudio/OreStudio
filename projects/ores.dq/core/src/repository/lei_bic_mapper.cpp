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
#include "ores.dq.core/repository/lei_bic_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq.api/domain/lei_bic.hpp"
#include "ores.dq.api/domain/lei_bic_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/lei_bic_entity.hpp"
#include "ores.logging/boost_severity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <vector>

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::lei_bic lei_bic_mapper::map(const lei_bic_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::lei_bic r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.lei = v.lei;
    r.bic = v.bic;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

lei_bic_entity lei_bic_mapper::map(const domain::lei_bic& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    lei_bic_entity r;
    r.lei = v.lei;
    r.tenant_id = v.tenant_id.to_string();
    r.bic = v.bic;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::lei_bic> lei_bic_mapper::map(const std::vector<lei_bic_entity>& v) {
    return map_vector<lei_bic_entity, domain::lei_bic>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<lei_bic_entity> lei_bic_mapper::map(const std::vector<domain::lei_bic>& v) {
    return map_vector<domain::lei_bic, lei_bic_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
