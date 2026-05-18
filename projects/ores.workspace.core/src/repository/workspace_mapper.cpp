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

#include <stdexcept>
#include <boost/uuid/string_generator.hpp>
#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::workspace::repository {

using namespace ores::database::repository;

domain::workspace workspace_mapper::map(const workspace_entity& e) {
    domain::workspace ws;

    boost::uuids::string_generator gen;
    ws.id = gen(e.id.value());
    ws.version = e.version;
    ws.name = e.name;
    ws.description = e.description;
    ws.source_path = e.source_path;

    if (e.parent_workspace_id)
        ws.parent_workspace_id = gen(*e.parent_workspace_id);

    if (e.scope_portfolio_id)
        ws.scope_portfolio_id = gen(*e.scope_portfolio_id);

    ws.owner_id = gen(e.owner_id);
    ws.status_code = e.status_code;
    ws.modified_by = e.modified_by;
    ws.performed_by = e.performed_by;
    ws.change_reason_code = e.change_reason_code;
    ws.change_commentary = e.change_commentary;

    if (!e.valid_from)
        throw std::logic_error("Cannot map workspace entity with null valid_from.");
    ws.recorded_at = timestamp_to_timepoint(*e.valid_from);

    return ws;
}

}
