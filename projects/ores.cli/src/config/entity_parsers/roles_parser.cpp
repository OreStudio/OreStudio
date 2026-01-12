/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.cli/config/entity_parsers/roles_parser.hpp"

#include "ores.cli/config/parser_helpers.hpp"
#include "ores.cli/config/entity.hpp"

namespace ores::cli::config::entity_parsers {

std::optional<options>
handle_roles_command(bool has_help,
    const boost::program_options::parsed_options& po,
    std::ostream& info,
    boost::program_options::variables_map& vm) {

    const parser_helpers::simple_entity_config cfg {
        .name = "roles",
        .description = "Manage roles",
        .entity_value = entity::roles,
        .list_description = "List roles as JSON or table",
        .delete_description = "Delete a role by ID"
    };

    return parser_helpers::handle_simple_entity_command(cfg, has_help, po, info, vm);
}

}
