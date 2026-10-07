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
#include "ores.iam.core/repository/database_info_lookups.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::iam::repository {

using namespace ores::logging;

namespace {

inline static std::string_view logger_name = "ores.iam.repository.database_info_lookups";

static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

}

messaging::database_info read_database_info(const ores::database::context& ctx) {
    try {
        const auto rows = ores::database::repository::execute_raw_multi_column_query(
            ctx,
            "select schema_fingerprint, build_environment, git_commit, created_at "
            "from ores_database_info_tbl order by created_at desc limit 1",
            lg(),
            "Reading the database's recorded build");
        if (rows.empty()) {
            BOOST_LOG_SEV(lg(), warn)
                << "ores_database_info_tbl holds no row; the database was not built by "
                   "compass db recreate";
            return {};
        }
        const auto& row = rows.front();
        return messaging::database_info{.fingerprint = row[0].value_or(""),
                                        .environment = row[1].value_or(""),
                                        .commit = row[2].value_or(""),
                                        .created = row[3].value_or("")};
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Could not read ores_database_info_tbl: " << e.what();
        return {};
    }
}

}
