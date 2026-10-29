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

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::dq::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a publication in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct publication_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_dq_dataset_publications_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    boost::uuids::uuid dataset_id;
    std::string dataset_code;
    std::string mode;
    std::string target_table;
    std::int64_t records_inserted;
    std::int64_t records_updated;
    std::int64_t records_skipped;
    std::int64_t records_deleted;
    std::string published_by;
    db_timestamp published_at;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = publication_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("dataset_id", &e::dataset_id) |
               query_builder::bind("dataset_code", &e::dataset_code) |
               query_builder::bind("mode", &e::mode) |
               query_builder::bind("target_table", &e::target_table) |
               query_builder::bind("records_inserted", &e::records_inserted) |
               query_builder::bind("records_updated", &e::records_updated) |
               query_builder::bind("records_skipped", &e::records_skipped) |
               query_builder::bind("records_deleted", &e::records_deleted) |
               query_builder::bind("published_by", &e::published_by) |
               query_builder::bind("published_at", &e::published_at);
    }
};

std::ostream& operator<<(std::ostream& s, const publication_entity& v);

}

#endif
