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
#ifndef ORES_DQ_REPOSITORY_PUBLICATION_ENTITY_HPP
#define ORES_DQ_REPOSITORY_PUBLICATION_ENTITY_HPP

#include <string>
#include <cstdint>
#include "sqlgen/PrimaryKey.hpp"
#include "sqlgen/Timestamp.hpp"

namespace ores::dq::repository {

/**
 * @brief Database entity for the dq_publications_tbl table.
 */
struct publication_entity final {
    constexpr static const char* schema = "ores";
    constexpr static const char* tablename = "dq_publications_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string dataset_id;
    std::string dataset_code;
    std::string mode;
    std::string target_table;
    std::int64_t records_inserted = 0;
    std::int64_t records_updated = 0;
    std::int64_t records_skipped = 0;
    std::int64_t records_deleted = 0;
    std::string published_by;
    sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> published_at;
};

std::ostream& operator<<(std::ostream& s, const publication_entity& v);

}

#endif
