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
#ifndef ORES_WORKSPACE_REPOSITORY_WORKSPACE_ENTITY_HPP
#define ORES_WORKSPACE_REPOSITORY_WORKSPACE_ENTITY_HPP

#include <string>
#include <optional>
#include <ostream>
#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::workspace::repository {

using db_timestamp = ores::database::repository::db_timestamp;

struct workspace_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_workspaces_tbl";

    sqlgen::PrimaryKey<std::string> id;
    int version = 0;
    std::string name;
    std::string description;
    std::string source_path;
    std::optional<std::string> parent_workspace_id;
    std::optional<std::string> scope_portfolio_id;
    std::string owner_id;
    std::string status_code;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::optional<db_timestamp> valid_from;
};

std::ostream& operator<<(std::ostream& s, const workspace_entity& v);

}

#endif
