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
#ifndef ORES_DATABASE_SERVICE_SERVICE_ACCOUNTS_HPP
#define ORES_DATABASE_SERVICE_SERVICE_ACCOUNTS_HPP

#include <string_view>

namespace ores::database::service::service_accounts {

/**
 * @brief Username constants for system service accounts.
 *
 * These must match the usernames created by
 * projects/ores.sql/populate/iam/iam_service_accounts_populate.sql
 * and the PostgreSQL roles in setup_user.sql.
 */

constexpr std::string_view ddl      = "ores_ddl_user";
constexpr std::string_view cli      = "ores_cli_user";
constexpr std::string_view wt       = "ores_wt_user";
constexpr std::string_view comms    = "ores_comms_user";
constexpr std::string_view http     = "ores_http_user";
constexpr std::string_view test_ddl = "ores_test_ddl_user";
constexpr std::string_view test_dml = "ores_test_dml_user";

}

#endif
