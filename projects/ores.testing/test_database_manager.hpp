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
#ifndef ORES_TESTING_TEST_DATABASE_MANAGER_HPP
#define ORES_TESTING_TEST_DATABASE_MANAGER_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>

namespace ores::testing {

/**
 * @brief Manages isolated test databases for parallel test execution.
 *
 * This class provides utilities for creating and destroying unique test
 * databases for each test process. This allows multiple test processes
 * to run concurrently without interference.
 *
 * Each test database is created from the oresdb_template database, which
 * must be pre-configured with the full schema.
 */
class test_database_manager {
public:
    /**
     * @brief Generates a unique database name for this test process.
     *
     * The database name is based on the process ID and a random suffix
     * to ensure uniqueness: oresdb_test_{pid}_{random}
     *
     * @return A unique database name string
     */
    static std::string generate_test_database_name();

    /**
     * @brief Creates a test database from the oresdb_template.
     *
     * This method connects to the postgres database (admin database) and
     * executes CREATE DATABASE with the template parameter.
     *
     * @param db_name The name of the database to create
     * @throws std::runtime_error if database creation fails
     */
    static void create_test_database(const std::string& db_name);

    /**
     * @brief Drops the test database.
     *
     * This method connects to the postgres database and executes
     * DROP DATABASE. It terminates any active connections first.
     *
     * @param db_name The name of the database to drop
     */
    static void drop_test_database(const std::string& db_name);

    /**
     * @brief Sets the TEST_ORES_DB_DATABASE environment variable.
     *
     * This ensures that database_fixture and all tests use the
     * isolated test database.
     *
     * @param db_name The database name to set
     */
    static void set_test_database_env(const std::string& db_name);
};

}

#endif
