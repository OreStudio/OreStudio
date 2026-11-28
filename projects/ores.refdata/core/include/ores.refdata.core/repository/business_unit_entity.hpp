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
#ifndef ORES_REFDATA_CORE_REPOSITORY_BUSINESS_UNIT_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_BUSINESS_UNIT_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/change_audit.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a business unit in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 *
 * The change audit is one member rather than four, because the model states
 * those columns as a group: =change_audit= holds the same four in their column
 * types, and the mapper copies each to the domain's audit record.
 */
struct business_unit_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_business_units_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid party_id;


    std::string unit_name;

    std::optional<boost::uuids::uuid> parent_business_unit_id;
    std::optional<std::string> unit_code;
    std::optional<std::string> business_centre_code;
    std::optional<boost::uuids::uuid> unit_type_id;
    std::string status;
    bool restricts_product_types = false;
    ores::database::repository::change_audit audit;
    db_timestamp valid_from = db_timestamp::max();
    db_timestamp valid_to = db_timestamp::max();

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = business_unit_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("unit_name", &e::unit_name) |
               query_builder::bind("parent_business_unit_id", &e::parent_business_unit_id) |
               query_builder::bind("unit_code", &e::unit_code) |
               query_builder::bind("business_centre_code", &e::business_centre_code) |
               query_builder::bind("unit_type_id", &e::unit_type_id) |
               query_builder::bind("status", &e::status) |
               query_builder::bind("restricts_product_types", &e::restricts_product_types) |
               query_builder::bind("modified_by", &e::audit.modified_by) |
               query_builder::bind("performed_by", &e::audit.performed_by) |
               query_builder::bind("change_reason_code", &e::audit.change_reason_code) |
               query_builder::bind("change_commentary", &e::audit.change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

ORES_REFDATA_CORE_EXPORT std::ostream& operator<<(std::ostream& s, const business_unit_entity& v);

}

#endif
