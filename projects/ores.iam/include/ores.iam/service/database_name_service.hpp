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
#ifndef ORES_IAM_SERVICE_DATABASE_NAME_SERVICE_HPP
#define ORES_IAM_SERVICE_DATABASE_NAME_SERVICE_HPP

#include <string>
#include <vector>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::iam::service {

/**
 * @brief Service for generating whimsical database names.
 *
 * The database_name_service generates Heroku-style memorable names for tenant
 * databases. Names follow the pattern: ores_<adjective>_<noun>[_<number>]
 *
 * Examples:
 *   - ores_silent_meadow
 *   - ores_autumn_frost
 *   - ores_crimson_dawn_4217
 *
 * The service delegates to PostgreSQL functions defined in
 * whimsical_names_create.sql, ensuring a single source of truth for the
 * word lists and generation logic.
 *
 * Usage patterns:
 *   - Local development: generate_database_name() without suffix
 *   - Production/C++ provisioning: generate_database_name(true) with suffix
 *   - Guaranteed unique: generate_unique_database_name()
 */
class database_name_service {
private:
    inline static std::string_view logger_name =
        "ores.iam.service.database_name_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs a database_name_service with database context.
     *
     * @param ctx The database context for executing SQL functions
     */
    explicit database_name_service(database::context ctx);

    /**
     * @brief Generates a whimsical name without prefix.
     *
     * @return A name like "silent_meadow" or "autumn_frost"
     */
    [[nodiscard]] std::string generate_whimsical_name();

    /**
     * @brief Generates a whimsical name with optional numeric suffix.
     *
     * @param with_suffix If true, appends a 4-digit number (e.g., "_4217")
     * @return A name like "silent_meadow" or "silent_meadow_4217"
     */
    [[nodiscard]] std::string generate_whimsical_name(bool with_suffix);

    /**
     * @brief Generates a database name with the 'ores_' prefix.
     *
     * @param with_suffix If true, appends a 4-digit number
     * @return A name like "ores_silent_meadow" or "ores_silent_meadow_4217"
     */
    [[nodiscard]] std::string generate_database_name(bool with_suffix = false);

    /**
     * @brief Generates a unique database name not in the provided list.
     *
     * Attempts to generate a name without suffix first. If all attempts
     * collide with existing names, falls back to adding a numeric suffix.
     *
     * @param existing_names List of already-used database names to avoid
     * @param max_attempts Maximum attempts before adding suffix (default 10)
     * @return A unique database name not in existing_names
     */
    [[nodiscard]] std::string generate_unique_database_name(
        const std::vector<std::string>& existing_names = {},
        int max_attempts = 10);

    /**
     * @brief Generates a unique database name by checking the server.
     *
     * Queries all existing databases on the PostgreSQL server that match
     * the ores_* pattern and generates a name that doesn't conflict.
     *
     * @return A unique database name not conflicting with any ores_* database
     */
    [[nodiscard]] std::string generate_unique_database_name_from_server();

private:
    database::context ctx_;

    /**
     * @brief Executes a SQL function that returns a single string.
     */
    [[nodiscard]] std::string execute_scalar_string_query(const std::string& sql);
};

}

#endif
