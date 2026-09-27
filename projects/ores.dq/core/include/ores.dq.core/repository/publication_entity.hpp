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
 * Template: cpp_domain_type_entity.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_DQ_CORE_REPOSITORY_PUBLICATION_ENTITY_HPP
#define ORES_DQ_CORE_REPOSITORY_PUBLICATION_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::dq::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a publication in the database.
 */
struct publication_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_dq_dataset_publications_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    std::string dataset_id;
    std::string dataset_code;
    std::string mode;
    std::string target_table;
    std::int64_t records_inserted;
    std::int64_t records_updated;
    std::int64_t records_skipped;
    std::int64_t records_deleted;
    std::string published_by;
    std::string published_at;
};

std::ostream& operator<<(std::ostream& s, const publication_entity& v);

}

#endif
