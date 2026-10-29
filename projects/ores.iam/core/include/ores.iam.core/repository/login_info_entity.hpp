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
#ifndef ORES_IAM_CORE_REPOSITORY_LOGIN_INFO_ENTITY_HPP
#define ORES_IAM_CORE_REPOSITORY_LOGIN_INFO_ENTITY_HPP

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
 * @brief Represents a login info in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct login_info_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_login_info_tbl";

    boost::uuids::uuid account_id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    std::string last_ip;
    std::string last_attempt_ip;
    int failed_logins = 0;
    bool locked = false;
    std::optional<db_timestamp> last_login;
    bool online = false;
    bool password_reset_required = false;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = login_info_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("account_id", &e::account_id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind_text("last_ip", &e::last_ip) |
               query_builder::bind_text("last_attempt_ip", &e::last_attempt_ip) |
               query_builder::bind("failed_logins", &e::failed_logins) |
               query_builder::bind("locked", &e::locked) |
               query_builder::bind("last_login", &e::last_login) |
               query_builder::bind("online", &e::online) |
               query_builder::bind("password_reset_required", &e::password_reset_required);
    }
};

std::ostream& operator<<(std::ostream& s, const login_info_entity& v);

}

#endif
