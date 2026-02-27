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
#ifndef ORES_DATABASE_DOMAIN_DATABASE_INFO_HPP
#define ORES_DATABASE_DOMAIN_DATABASE_INFO_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::database::domain {

/**
 * @brief Build and schema metadata recorded when the database was created or recreated.
 *
 * Stores the schema version, git commit, and build environment inserted at
 * database creation time. Used to correlate service behaviour with the
 * database schema in use. Contains exactly one row.
 */
struct database_info final {
    /**
     * @brief Fixed UUID identifying the singleton database info record.
     *
     * A well-known UUID inserted by the creation script.
     */
    boost::uuids::uuid id;

    /**
     * @brief Database schema version matching the application version.
     *
     * Set to ORES_VERSION (e.g. '0.0.13') at database creation time.
     */
    std::string schema_version;

    /**
     * @brief Build environment identifier.
     *
     * e.g. 'local', 'production', 'staging'. Prefix from ORES_BUILD_INFO.
     */
    std::string build_environment;

    /**
     * @brief Short git commit hash of the code that created this database.
     *
     * e.g. 'c7681ac4' or 'c7681ac4-dirty'. From ORES_BUILD_INFO.
     */
    std::string git_commit;

    /**
     * @brief Date and time of the git commit that created this database.
     *
     * e.g. '2026/02/25 22:43:31'. From ORES_BUILD_INFO.
     */
    std::string git_date;

    /**
     * @brief Timestamp when this record was created.
     */
    std::chrono::system_clock::time_point created_at;
};

}

#endif
