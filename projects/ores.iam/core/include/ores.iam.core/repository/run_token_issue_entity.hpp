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
#ifndef ORES_IAM_CORE_REPOSITORY_RUN_TOKEN_ISSUE_ENTITY_HPP
#define ORES_IAM_CORE_REPOSITORY_RUN_TOKEN_ISSUE_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::iam::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a run token issue in the database.
 */
struct run_token_issue_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_run_token_issues_tbl";

    sqlgen::PrimaryKey<std::string> id;
    sqlgen::PrimaryKey<std::string> issued_at;
    std::string tenant_id;
    std::string party_id;
    std::string grant_id;
    std::string run_id;
    std::string service;
    std::string grantor_account_id;
    std::string outcome;
    std::string reason;
};

std::ostream& operator<<(std::ostream& s, const run_token_issue_entity& v);

}

#endif
