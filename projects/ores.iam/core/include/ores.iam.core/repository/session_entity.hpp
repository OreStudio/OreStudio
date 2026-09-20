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
#ifndef ORES_IAM_CORE_REPOSITORY_SESSION_ENTITY_HPP
#define ORES_IAM_CORE_REPOSITORY_SESSION_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::iam::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a session in the database.
 */
struct session_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_sessions_tbl";

    sqlgen::PrimaryKey<std::string> id;
    sqlgen::PrimaryKey<std::string> start_time;
    std::string tenant_id;
    std::string account_id;
    std::string end_time = "";
    std::string client_ip;
    std::string client_identifier = "";
    std::uint16_t client_version_major = 0;
    std::uint16_t client_version_minor = 0;
    std::uint64_t bytes_sent = 0;
    std::uint64_t bytes_received = 0;
    std::string country_code = "";
    std::string protocol = "binary";
};

std::ostream& operator<<(std::ostream& s, const session_entity& v);

}

#endif
