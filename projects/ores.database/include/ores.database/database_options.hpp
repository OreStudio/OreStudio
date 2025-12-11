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
#ifndef ORES_DATABASE_DATABASE_OPTIONS_HPP
#define ORES_DATABASE_DATABASE_OPTIONS_HPP

#include <iosfwd>
#include <string>
#include <rfl.hpp>
#include <sqlgen/postgres.hpp>

namespace ores::database {

/**
 * @brief Configuration for database connection.
 */
struct database_options final {
    /**
     * @brief Database user name.
     */
    std::string user;
    /**
     * @brief Password for the user.
     */
    rfl::Skip<std::string> password;
    /**
     * @brief Host to connect to.
     */
    std::string host = "localhost";
    /**
     * @brief Database to connect to.
     */
    std::string database;
    /**
     * @brief Port the database is listening on.
     */
    int port = 5432;
};

std::ostream& operator<<(std::ostream& s, const database_options& v);

/**
 * @brief Converts database_options to sqlgen::postgres::Credentials.
 *
 * @param opts The database options to convert.
 * @return The corresponding sqlgen credentials.
 */
sqlgen::postgres::Credentials to_credentials(const database_options& opts);

}

#endif
