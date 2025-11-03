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
#ifndef ORES_UTILITY_TEST_DATABASE_FIXTURE_HPP
#define ORES_UTILITY_TEST_DATABASE_FIXTURE_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/repository/context.hpp"

namespace ores::utility::test {

/**
 * @brief Provides database setup and cleanup utilities for tests.
 *
 * This class uses environment variables to configure the database connection:
 * - ORES_DB_USER: Database user name (default: "ores")
 * - ORES_DB_PASSWORD: Database password (default: "")
 * - ORES_DB_HOST: Database host (default: "localhost")
 * - ORES_DB_DATABASE: Database name (default: "oresdb")
 * - ORES_DB_PORT: Database port (default: 5432)
 */
class database_fixture {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.utility.test.database_fixture");
        return instance;
    }

public:
    /**
     * @brief Database configuration options.
     */
    struct database_options {
        std::string user;
        std::string password;
        std::string host;
        std::string database;
        int port;
    };

    database_fixture();

    /**
     * @brief Creates database options from environment variables.
     */
    static database_options make_database_options();

    /**
     * @brief Creates a database context from environment variables.
     */
    static repository::context make_context();

    /**
     * @brief Truncates the specified table.
     *
     * @param table_name Fully qualified table name (e.g., "oresdb.accounts")
     */
    void truncate_table(const std::string& table_name);

    /**
     * @brief Gets the database context.
     */
    repository::context& get_context() { return context_; }

private:
    repository::context context_;
};

}

#endif
