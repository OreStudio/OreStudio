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
#ifndef ORES_DQ_REPOSITORY_CODING_SCHEME_ENTITY_HPP
#define ORES_DQ_REPOSITORY_CODING_SCHEME_ENTITY_HPP

#include <string>
#include <optional>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::dq::repository {

/**
 * @brief Represents a coding_scheme in the database.
 */
struct coding_scheme_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_dq_coding_schemes_tbl";

    sqlgen::PrimaryKey<std::string> code;
    std::string tenant_id;
    int version = 0;
    std::string name;
    std::string authority_type;
    std::string subject_area_name;
    std::string domain_name;
    std::optional<std::string> uri;
    std::string description;
    std::string modified_by;
    std::string change_commentary;
    sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> valid_from = "9999-12-31 23:59:59";
    sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const coding_scheme_entity& v);

}

#endif
