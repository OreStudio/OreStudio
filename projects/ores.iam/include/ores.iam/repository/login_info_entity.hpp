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
#ifndef ORES_IAM_REPOSITORY_LOGIN_INFO_ENTITY_HPP
#define ORES_IAM_REPOSITORY_LOGIN_INFO_ENTITY_HPP

#include <string>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::iam::repository {

/**
 * @brief Represents login tracking information in the database.
 */
struct login_info_entity {
    constexpr static const char* schema = "oresdb";
    constexpr static const char* tablename = "login_info";

    sqlgen::PrimaryKey<std::string> account_id;
    std::string last_ip;
    std::string last_attempt_ip;
    int failed_logins;
    int locked;
    sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> last_login = "9999-12-31 23:59:59";
    int online;
    int password_reset_required;
};

std::ostream& operator<<(std::ostream& s, const login_info_entity& v);

}

#endif
