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
#ifndef ORES_DQ_REPOSITORY_ARTEFACT_TYPE_ENTITY_HPP
#define ORES_DQ_REPOSITORY_ARTEFACT_TYPE_ENTITY_HPP

#include <string>
#include <optional>
#include "sqlgen/PrimaryKey.hpp"

namespace ores::dq::repository {

/**
 * @brief Represents an artefact_type in the database.
 *
 * This is a simple lookup table - no bitemporal support.
 */
struct artefact_type_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_dq_artefact_types_tbl";

    sqlgen::PrimaryKey<std::string> code;
    std::string tenant_id;
    std::string name;
    std::optional<std::string> description;
    std::optional<std::string> artefact_table;
    std::optional<std::string> target_table;
    std::optional<std::string> populate_function;
    int display_order = 0;
};

std::ostream& operator<<(std::ostream& s, const artefact_type_entity& v);

}

#endif
