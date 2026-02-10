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
#ifndef ORES_REFDATA_REPOSITORY_PARTY_ENTITY_HPP
#define ORES_REFDATA_REPOSITORY_PARTY_ENTITY_HPP

#include <string>
#include <optional>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::refdata::repository {

/**
 * @brief Represents a party in the database.
 */
struct party_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_parties_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    std::string full_name;
    std::string short_code;
    std::string party_category;
    std::string party_type;
    std::optional<std::string> parent_party_id;
    std::optional<std::string> business_center_code;
    std::string status;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> valid_from = "9999-12-31 23:59:59";
    sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const party_entity& v);

}

#endif
