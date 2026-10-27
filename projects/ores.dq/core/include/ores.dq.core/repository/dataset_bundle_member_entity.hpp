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
#ifndef ORES_DQ_CORE_REPOSITORY_DATASET_BUNDLE_MEMBER_ENTITY_HPP
#define ORES_DQ_CORE_REPOSITORY_DATASET_BUNDLE_MEMBER_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::dq::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a dataset bundle member in the database.
 *
 * Junction table with composite primary key (bundle_code, dataset_code, valid_from).
 */
struct dataset_bundle_member_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_dq_dataset_bundle_members_tbl";

    std::string bundle_code;
    std::string tenant_id;
    std::string dataset_code;
    int version = 0;
    int display_order = 0;
    bool optional;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
    std::optional<std::string> closed_by;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = dataset_bundle_member_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("bundle_code", &e::bundle_code) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("dataset_code", &e::dataset_code) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("display_order", &e::display_order) |
               query_builder::bind("optional", &e::optional) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to) |
               query_builder::bind("closed_by", &e::closed_by);
    }
};

std::ostream& operator<<(std::ostream& s, const dataset_bundle_member_entity& v);

}

#endif
