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
#include "ores.refdata/repository/portfolio_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata/domain/portfolio_json_io.hpp" // IWYU pragma: keep.

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::portfolio
portfolio_mapper::map(const portfolio_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::portfolio r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.name = v.name;
    r.description = v.description;
    if (v.parent_portfolio_id.has_value() && !v.parent_portfolio_id->empty())
        r.parent_portfolio_id = boost::lexical_cast<boost::uuids::uuid>(*v.parent_portfolio_id);
    if (v.owner_unit_id.has_value() && !v.owner_unit_id->empty())
        r.owner_unit_id = boost::lexical_cast<boost::uuids::uuid>(*v.owner_unit_id);
    r.purpose_type = v.purpose_type;
    r.aggregation_ccy = v.aggregation_ccy;
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

portfolio_entity
portfolio_mapper::map(const domain::portfolio& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    portfolio_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.name = v.name;
    r.description = v.description;
    if (v.parent_portfolio_id)
        r.parent_portfolio_id = boost::uuids::to_string(*v.parent_portfolio_id);
    if (v.owner_unit_id)
        r.owner_unit_id = boost::uuids::to_string(*v.owner_unit_id);
    r.purpose_type = v.purpose_type;
    r.aggregation_ccy = v.aggregation_ccy;
    r.is_virtual = v.is_virtual;
    r.status = v.status;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::portfolio>
portfolio_mapper::map(const std::vector<portfolio_entity>& v) {
    return map_vector<portfolio_entity, domain::portfolio>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<portfolio_entity>
portfolio_mapper::map(const std::vector<domain::portfolio>& v) {
    return map_vector<domain::portfolio, portfolio_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
