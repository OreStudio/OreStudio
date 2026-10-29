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
#ifndef ORES_IAM_CORE_REPOSITORY_SEED_PROFILE_ENTITY_HPP
#define ORES_IAM_CORE_REPOSITORY_SEED_PROFILE_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::iam::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a seed profile in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct seed_profile_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_seed_profiles_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;

    std::string code;

    std::string name;
    std::string summary;
    std::string audience;
    std::string bullets_json;
    std::string tenant_type;
    std::string tenant_name;
    std::string tenant_code;
    std::optional<std::string> tenant_hostname;
    std::string admin_username;
    std::string admin_email;
    bool inherits_admin_password = false;
    bool force_password_change = false;
    int display_order = 0;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = db_timestamp::max();
    db_timestamp valid_to = db_timestamp::max();

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = seed_profile_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) | query_builder::bind("code", &e::code) |
               query_builder::bind("name", &e::name) | query_builder::bind("summary", &e::summary) |
               query_builder::bind("audience", &e::audience) |
               query_builder::bind_text("bullets_json", &e::bullets_json) |
               query_builder::bind("tenant_type", &e::tenant_type) |
               query_builder::bind("tenant_name", &e::tenant_name) |
               query_builder::bind("tenant_code", &e::tenant_code) |
               query_builder::bind("tenant_hostname", &e::tenant_hostname) |
               query_builder::bind("admin_username", &e::admin_username) |
               query_builder::bind("admin_email", &e::admin_email) |
               query_builder::bind("inherits_admin_password", &e::inherits_admin_password) |
               query_builder::bind("force_password_change", &e::force_password_change) |
               query_builder::bind("display_order", &e::display_order) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const seed_profile_entity& v);

}

#endif
