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
#include "ores.connections/repository/sqlite_context.hpp"

#include <stdexcept>

namespace ores::connections::repository {

sqlite_context::sqlite_context(std::filesystem::path db_path)
    : db_path_(std::move(db_path)) {}

rfl::Result<sqlite_context::connection_ref> sqlite_context::connect() const {
    return sqlgen::sqlite::connect(db_path_.string());
}

void sqlite_context::initialize_schema() {
    auto conn_result = connect();
    if (!conn_result) {
        throw std::runtime_error("Failed to connect to SQLite database: " +
                                 db_path_.string());
    }

    auto& conn = *conn_result;

    // Enable foreign keys
    conn->execute("PRAGMA foreign_keys = ON");

    // Create folders table
    const std::string create_folders = R"(
        CREATE TABLE IF NOT EXISTS folders (
            id TEXT PRIMARY KEY,
            name TEXT NOT NULL,
            parent_id TEXT,
            description TEXT,
            FOREIGN KEY (parent_id) REFERENCES folders(id) ON DELETE CASCADE
        )
    )";

    // Create tags table
    const std::string create_tags = R"(
        CREATE TABLE IF NOT EXISTS tags (
            id TEXT PRIMARY KEY,
            name TEXT NOT NULL UNIQUE
        )
    )";

    // Create environments table (pure host/port, no credentials)
    const std::string create_environments = R"(
        CREATE TABLE IF NOT EXISTS environments (
            id TEXT PRIMARY KEY,
            folder_id TEXT,
            name TEXT NOT NULL,
            host TEXT NOT NULL,
            port INTEGER NOT NULL,
            description TEXT,
            FOREIGN KEY (folder_id) REFERENCES folders(id) ON DELETE SET NULL
        )
    )";

    // Create connections table (credentials, optional link to environment)
    const std::string create_connections = R"(
        CREATE TABLE IF NOT EXISTS connections (
            id TEXT PRIMARY KEY,
            folder_id TEXT,
            environment_id TEXT,
            name TEXT NOT NULL,
            host TEXT,
            port INTEGER,
            username TEXT NOT NULL,
            encrypted_password TEXT,
            description TEXT,
            FOREIGN KEY (folder_id) REFERENCES folders(id) ON DELETE SET NULL,
            FOREIGN KEY (environment_id) REFERENCES environments(id) ON DELETE SET NULL
        )
    )";

    // Create environment_tags junction table (for pure environments)
    const std::string create_environment_tags = R"(
        CREATE TABLE IF NOT EXISTS environment_tags (
            environment_id TEXT NOT NULL,
            tag_id TEXT NOT NULL,
            PRIMARY KEY (environment_id, tag_id),
            FOREIGN KEY (environment_id) REFERENCES environments(id) ON DELETE CASCADE,
            FOREIGN KEY (tag_id) REFERENCES tags(id) ON DELETE CASCADE
        )
    )";

    // Create connection_tags junction table (for connections with credentials)
    const std::string create_connection_tags = R"(
        CREATE TABLE IF NOT EXISTS connection_tags (
            connection_id TEXT NOT NULL,
            tag_id TEXT NOT NULL,
            PRIMARY KEY (connection_id, tag_id),
            FOREIGN KEY (connection_id) REFERENCES connections(id) ON DELETE CASCADE,
            FOREIGN KEY (tag_id) REFERENCES tags(id) ON DELETE CASCADE
        )
    )";

    // Execute schema creation
    conn->execute(create_folders);
    conn->execute(create_tags);
    conn->execute(create_environments);
    conn->execute(create_connections);
    conn->execute(create_environment_tags);
    conn->execute(create_connection_tags);

    // Migration: Add description column to folders if it doesn't exist
    try {
        conn->execute("ALTER TABLE folders ADD COLUMN description TEXT");
    } catch (...) {
        // Column already exists, ignore
    }

    // Migration: migrate old server_environments → connections if old table exists.
    // We probe the table directly: execute() returns an error Result when the
    // table is absent, so checking the Result avoids exception-based control flow.
    if (conn->execute("SELECT 1 FROM server_environments LIMIT 0")) {
        // Copy rows from the old table into the new connections table.
        conn->execute(R"(
            INSERT OR IGNORE INTO connections
                (id, folder_id, environment_id, name, host, port,
                 username, encrypted_password, description)
            SELECT id, folder_id, NULL, name, host, port,
                   username, encrypted_password, description
            FROM server_environments
        )");

        // Migrate any tags. environment_tags is guaranteed to exist (created
        // above), so this INSERT is always safe; it simply inserts no rows if
        // there were no matching tags.
        conn->execute(R"(
            INSERT OR IGNORE INTO connection_tags (connection_id, tag_id)
            SELECT et.environment_id, et.tag_id
            FROM environment_tags et
            WHERE et.environment_id IN (SELECT id FROM server_environments)
        )");

        // Drop old tables and recreate environment_tags with the new schema
        // (referencing environments instead of server_environments).
        conn->execute("DROP TABLE IF EXISTS environment_tags");
        conn->execute("DROP TABLE IF EXISTS server_environments");
        conn->execute(create_environment_tags);
    }
}

void sqlite_context::purge_all_data() {
    auto conn_result = connect();
    if (!conn_result) {
        throw std::runtime_error("Failed to connect to SQLite database: " +
                                 db_path_.string());
    }

    auto& conn = *conn_result;

    // Delete in order to respect foreign key constraints:
    // 1. Junction tables first (reference both parent tables)
    // 2. connections and environments (reference folders)
    // 3. folders (self-referential, CASCADE handles children)
    // 4. tags (no dependencies)
    conn->execute("DELETE FROM connection_tags");
    conn->execute("DELETE FROM environment_tags");
    conn->execute("DELETE FROM connections");
    conn->execute("DELETE FROM environments");
    conn->execute("DELETE FROM folders");
    conn->execute("DELETE FROM tags");
}

}
