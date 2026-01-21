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

    // Create server_environments table
    const std::string create_environments = R"(
        CREATE TABLE IF NOT EXISTS server_environments (
            id TEXT PRIMARY KEY,
            folder_id TEXT,
            name TEXT NOT NULL,
            host TEXT NOT NULL,
            port INTEGER NOT NULL,
            username TEXT NOT NULL,
            encrypted_password TEXT,
            description TEXT,
            FOREIGN KEY (folder_id) REFERENCES folders(id) ON DELETE SET NULL
        )
    )";

    // Create environment_tags junction table
    const std::string create_environment_tags = R"(
        CREATE TABLE IF NOT EXISTS environment_tags (
            environment_id TEXT NOT NULL,
            tag_id TEXT NOT NULL,
            PRIMARY KEY (environment_id, tag_id),
            FOREIGN KEY (environment_id) REFERENCES server_environments(id) ON DELETE CASCADE,
            FOREIGN KEY (tag_id) REFERENCES tags(id) ON DELETE CASCADE
        )
    )";

    // Execute schema creation
    conn->execute(create_folders);
    conn->execute(create_tags);
    conn->execute(create_environments);
    conn->execute(create_environment_tags);

    // Migration: Add description column to folders if it doesn't exist
    // SQLite silently ignores duplicate column errors, but we catch anyway
    try {
        conn->execute("ALTER TABLE folders ADD COLUMN description TEXT");
    } catch (...) {
        // Column already exists, ignore
    }

    // Enable foreign keys
    conn->execute("PRAGMA foreign_keys = ON");
}

void sqlite_context::purge_all_data() {
    auto conn_result = connect();
    if (!conn_result) {
        throw std::runtime_error("Failed to connect to SQLite database: " +
                                 db_path_.string());
    }

    auto& conn = *conn_result;

    // Delete in order to respect foreign key constraints:
    // 1. environment_tags (references both environments and tags)
    // 2. server_environments (references folders)
    // 3. folders (self-referential, but CASCADE handles children)
    // 4. tags (no dependencies)
    conn->execute("DELETE FROM environment_tags");
    conn->execute("DELETE FROM server_environments");
    conn->execute("DELETE FROM folders");
    conn->execute("DELETE FROM tags");
}

}
