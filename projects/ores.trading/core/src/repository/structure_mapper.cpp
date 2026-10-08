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
#include "ores.trading.core/repository/structure_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/structure.hpp"
#include "ores.trading.api/domain/structure_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/structure_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::structure structure_mapper::map(const structure_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::structure r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.counterparty_id = boost::lexical_cast<boost::uuids::uuid>(v.counterparty_id);
    r.kind = v.kind;
    r.template_code = v.template_code.value_or("");
    r.parent_structure_id = v.parent_structure_id.has_value() ?
                                boost::lexical_cast<boost::uuids::uuid>(*v.parent_structure_id) :
                                boost::uuids::uuid{};

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

structure_entity structure_mapper::map(const domain::structure& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    structure_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.party_id = boost::uuids::to_string(v.party_id);
    r.counterparty_id = boost::uuids::to_string(v.counterparty_id);
    r.kind = v.kind;
    r.template_code = v.template_code.empty() ? std::nullopt : std::optional(v.template_code);
    r.parent_structure_id = v.parent_structure_id == boost::uuids::uuid{} ?
                                std::nullopt :
                                std::optional(boost::uuids::to_string(v.parent_structure_id));

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::structure> structure_mapper::map(const std::vector<structure_entity>& v) {
    return map_vector<structure_entity, domain::structure>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<structure_entity> structure_mapper::map(const std::vector<domain::structure>& v) {
    return map_vector<domain::structure, structure_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
