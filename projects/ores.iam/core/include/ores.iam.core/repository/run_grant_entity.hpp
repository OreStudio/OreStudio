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
#ifndef ORES_IAM_CORE_REPOSITORY_RUN_GRANT_ENTITY_HPP
#define ORES_IAM_CORE_REPOSITORY_RUN_GRANT_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::iam::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a run grant in the database.
 */
struct run_grant_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_run_grants_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;


    std::string resource;

    std::string grantor_account_id;

    std::string role_id;
    std::string audience;
    std::optional<int> max_runs;
    std::optional<db_timestamp> not_after;
    std::optional<db_timestamp> revoked_at;
    std::optional<std::string> revoked_by;
    std::optional<std::string> revoke_reason;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = run_grant_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("resource", &e::resource) |
               query_builder::bind("grantor_account_id", &e::grantor_account_id) |
               query_builder::bind("role_id", &e::role_id) |
               query_builder::bind("audience", &e::audience) |
               query_builder::bind("max_runs", &e::max_runs) |
               query_builder::bind("not_after", &e::not_after) |
               query_builder::bind("revoked_at", &e::revoked_at) |
               query_builder::bind("revoked_by", &e::revoked_by) |
               query_builder::bind("revoke_reason", &e::revoke_reason) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const run_grant_entity& v);

}

#endif
