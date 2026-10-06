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
#include "ores.dq.core/repository/netting_agreement_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq.api/domain/netting_agreement.hpp"
#include "ores.dq.api/domain/netting_agreement_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/netting_agreement_entity.hpp"
#include "ores.logging/boost_severity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <vector>

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::netting_agreement netting_agreement_mapper::map(const netting_agreement_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::netting_agreement r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.agreement_number = v.agreement_number.value();
    r.counterparty_lei = v.counterparty_lei;
    r.agreement_type = v.agreement_type;
    r.governing_law = v.governing_law;
    r.description = v.description;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

netting_agreement_entity netting_agreement_mapper::map(const domain::netting_agreement& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    netting_agreement_entity r;
    r.agreement_number = v.agreement_number;
    r.tenant_id = v.tenant_id.to_string();
    r.counterparty_lei = v.counterparty_lei;
    r.agreement_type = v.agreement_type;
    r.governing_law = v.governing_law;
    r.description = v.description;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::netting_agreement>
netting_agreement_mapper::map(const std::vector<netting_agreement_entity>& v) {
    return map_vector<netting_agreement_entity, domain::netting_agreement>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<netting_agreement_entity>
netting_agreement_mapper::map(const std::vector<domain::netting_agreement>& v) {
    return map_vector<domain::netting_agreement, netting_agreement_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
