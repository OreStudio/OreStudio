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
#include "ores.workspace.core/repository/workspace_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.workspace.api/domain/workspace_json_io.hpp" // IWYU pragma: keep.

namespace ores::workspace::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::workspace
workspace_mapper::map(const workspace_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::workspace r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.version = v.version;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.name = v.name;
    r.description = v.description.value_or("");
    r.source_path = v.source_path.value_or("");
    r.parent_workspace_id = v.parent_workspace_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.parent_workspace_id)) : std::nullopt;
    r.scope_portfolio_id = v.scope_portfolio_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.scope_portfolio_id)) : std::nullopt;
    r.owner_id = boost::lexical_cast<boost::uuids::uuid>(v.owner_id);
    r.status_code = v.status_code;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error("Cannot map entity with null valid_from to domain object.");
    r.recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

workspace_entity
workspace_mapper::map(const domain::workspace& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    workspace_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.party_id = boost::uuids::to_string(v.party_id);
    r.version = v.version;
    r.name = v.name;
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.source_path = v.source_path.empty() ? std::nullopt : std::optional(v.source_path);
    r.parent_workspace_id = v.parent_workspace_id.has_value() ? std::optional(boost::uuids::to_string(*v.parent_workspace_id)) : std::nullopt;
    r.scope_portfolio_id = v.scope_portfolio_id.has_value() ? std::optional(boost::uuids::to_string(*v.scope_portfolio_id)) : std::nullopt;
    r.owner_id = boost::uuids::to_string(v.owner_id);
    r.status_code = v.status_code;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::workspace>
workspace_mapper::map(const std::vector<workspace_entity>& v) {
    return map_vector<workspace_entity, domain::workspace>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<workspace_entity>
workspace_mapper::map(const std::vector<domain::workspace>& v) {
    return map_vector<domain::workspace, workspace_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
