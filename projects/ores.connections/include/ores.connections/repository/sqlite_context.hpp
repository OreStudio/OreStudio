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
#ifndef ORES_CONNECTIONS_REPOSITORY_SQLITE_CONTEXT_HPP
#define ORES_CONNECTIONS_REPOSITORY_SQLITE_CONTEXT_HPP

#include <filesystem>
#include <rfl/Result.hpp>
#include <sqlgen/Ref.hpp>
#include <sqlgen/sqlite.hpp>

namespace ores::connections::repository {

/**
 * @brief SQLite database context for connection management.
 *
 * Provides a simple connection context for the local SQLite database
 * that stores server environment bookmarks, folders, and tags.
 */
class sqlite_context final {
public:
    using connection_type = sqlgen::sqlite::Connection;
    using connection_ref = sqlgen::Ref<connection_type>;

    /**
     * @brief Construct context with database file path.
     * @param db_path Path to the SQLite database file.
     */
    explicit sqlite_context(std::filesystem::path db_path);

    /**
     * @brief Get a connection to the SQLite database.
     * @return Result containing the connection reference or an error.
     */
    rfl::Result<connection_ref> connect() const;

    /**
     * @brief Get the database file path.
     */
    const std::filesystem::path& db_path() const { return db_path_; }

    /**
     * @brief Initialize the database schema if it doesn't exist.
     */
    void initialize_schema();

    /**
     * @brief Delete all data from all tables.
     *
     * Efficiently purges the database using bulk DELETE statements.
     * Tables are deleted in order to respect foreign key constraints.
     */
    void purge_all_data();

private:
    std::filesystem::path db_path_;
};

}

#endif
