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
#include "ores.refdata.core/repository/netting_set_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/netting_set.hpp"
#include "ores.refdata.api/domain/netting_set_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/netting_set_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::netting_set netting_set_mapper::map(const netting_set_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::netting_set r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());

    r.code = v.code;

    r.netting_agreement_id =
        v.netting_agreement_id.has_value() ?
            std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.netting_agreement_id)) :
            std::nullopt;
    r.counterparty_id =
        v.counterparty_id.has_value() ?
            std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.counterparty_id)) :
            std::nullopt;
    r.party_id = v.party_id.has_value() ?
                     std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.party_id)) :
                     std::nullopt;
    r.call_type = v.call_type;
    r.initial_margin_type = v.initial_margin_type;
    r.risk_weight = v.risk_weight;
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

netting_set_entity netting_set_mapper::map(const domain::netting_set& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    netting_set_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;

    r.code = v.code;

    r.netting_agreement_id = v.netting_agreement_id.has_value() ?
                                 std::optional(boost::uuids::to_string(*v.netting_agreement_id)) :
                                 std::nullopt;
    r.counterparty_id = v.counterparty_id.has_value() ?
                            std::optional(boost::uuids::to_string(*v.counterparty_id)) :
                            std::nullopt;
    r.party_id =
        v.party_id.has_value() ? std::optional(boost::uuids::to_string(*v.party_id)) : std::nullopt;
    r.call_type = v.call_type;
    r.initial_margin_type = v.initial_margin_type;
    r.risk_weight = v.risk_weight;
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::netting_set> netting_set_mapper::map(const std::vector<netting_set_entity>& v) {
    return map_vector<netting_set_entity, domain::netting_set>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<netting_set_entity> netting_set_mapper::map(const std::vector<domain::netting_set>& v) {
    return map_vector<domain::netting_set, netting_set_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
