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
#include "ores.refdata.core/repository/portfolio_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/portfolio_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::portfolio portfolio_mapper::map(const portfolio_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::portfolio r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.workspace_id = boost::lexical_cast<boost::uuids::uuid>(v.workspace_id);
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);


    r.name = v.name;

    r.description = v.description.value_or("");
    r.parent_portfolio_id =
        v.parent_portfolio_id.has_value() ?
            std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.parent_portfolio_id)) :
            std::nullopt;
    r.owner_unit_id = v.owner_unit_id.has_value() ?
                          std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.owner_unit_id)) :
                          std::nullopt;
    r.purpose_type = v.purpose_type;
    r.aggregation_ccy = v.aggregation_ccy.value_or("");
    r.is_virtual = v.is_virtual;
    r.status = v.status;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

portfolio_entity portfolio_mapper::map(const domain::portfolio& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    portfolio_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.workspace_id = boost::uuids::to_string(v.workspace_id);
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);


    r.name = v.name;

    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.parent_portfolio_id = v.parent_portfolio_id.has_value() ?
                                std::optional(boost::uuids::to_string(*v.parent_portfolio_id)) :
                                std::nullopt;
    r.owner_unit_id = v.owner_unit_id.has_value() ?
                          std::optional(boost::uuids::to_string(*v.owner_unit_id)) :
                          std::nullopt;
    r.purpose_type = v.purpose_type;
    r.aggregation_ccy = v.aggregation_ccy.empty() ? std::nullopt : std::optional(v.aggregation_ccy);
    r.is_virtual = v.is_virtual;
    r.status = v.status;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::portfolio> portfolio_mapper::map(const std::vector<portfolio_entity>& v) {
    return map_vector<portfolio_entity, domain::portfolio>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<portfolio_entity> portfolio_mapper::map(const std::vector<domain::portfolio>& v) {
    return map_vector<domain::portfolio, portfolio_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
