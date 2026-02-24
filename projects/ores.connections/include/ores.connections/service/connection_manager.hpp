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
#ifndef ORES_CONNECTIONS_SERVICE_CONNECTION_MANAGER_HPP
#define ORES_CONNECTIONS_SERVICE_CONNECTION_MANAGER_HPP

#include <string>
#include <vector>
#include <optional>
#include <filesystem>
#include <boost/uuid/uuid.hpp>
#include "ores.connections/domain/folder.hpp"
#include "ores.connections/domain/tag.hpp"
#include "ores.connections/domain/connection.hpp"
#include "ores.connections/domain/connection_tag.hpp"
#include "ores.connections/domain/environment.hpp"
#include "ores.connections/domain/environment_tag.hpp"
#include "ores.connections/repository/sqlite_context.hpp"
#include "ores.connections/repository/folder_repository.hpp"
#include "ores.connections/repository/tag_repository.hpp"
#include "ores.connections/repository/connection_repository.hpp"
#include "ores.connections/repository/connection_tag_repository.hpp"
#include "ores.connections/repository/environment_repository.hpp"
#include "ores.connections/repository/environment_tag_repository.hpp"

namespace ores::connections::service {

/**
 * @brief High-level service for managing server connections and environments.
 *
 * Provides a unified API for managing:
 * - Pure environments (host + port, no credentials)
 * - Connections (credentials optionally linked to an environment)
 * - Folder organization and tag-based categorization
 * - Encrypted password storage
 */
class connection_manager final {
public:
    /**
     * @brief Resolved connection details for opening a login dialog.
     *
     * When a connection is linked to an environment, host/port are resolved
     * from the environment. The password is decrypted from storage.
     */
    struct resolved_connection {
        std::string host;
        int port{0};
        std::string username;
        std::string password;    ///< decrypted
        std::string name;
        std::optional<boost::uuids::uuid> environment_id;
        std::optional<std::string> environment_name;
    };

    /**
     * @brief Construct connection manager with database path and master password.
     * @param db_path Path to the SQLite database file.
     * @param master_password Master password for encryption/decryption.
     */
    connection_manager(const std::filesystem::path& db_path,
                       const std::string& master_password);

    // Folder operations
    void create_folder(const domain::folder& folder);
    void update_folder(const domain::folder& folder);
    void delete_folder(const boost::uuids::uuid& id);
    std::vector<domain::folder> get_all_folders();
    std::optional<domain::folder> get_folder(const boost::uuids::uuid& id);
    std::vector<domain::folder> get_root_folders();
    std::vector<domain::folder> get_child_folders(const boost::uuids::uuid& parent_id);

    // Tag operations
    void create_tag(const domain::tag& tag);
    void update_tag(const domain::tag& tag);
    void delete_tag(const boost::uuids::uuid& id);
    std::vector<domain::tag> get_all_tags();
    std::optional<domain::tag> get_tag(const boost::uuids::uuid& id);
    std::optional<domain::tag> get_tag_by_name(const std::string& name);

    // Environment operations (pure host/port, no credentials)
    void create_environment(const domain::environment& env);
    void update_environment(const domain::environment& env);
    void delete_environment(const boost::uuids::uuid& id);
    std::vector<domain::environment> get_all_environments();
    std::optional<domain::environment> get_environment(const boost::uuids::uuid& id);
    std::vector<domain::environment> get_environments_in_folder(
        const std::optional<boost::uuids::uuid>& folder_id);

    // Environment-tag operations
    void add_tag_to_environment(const boost::uuids::uuid& environment_id,
                                const boost::uuids::uuid& tag_id);
    void remove_tag_from_environment(const boost::uuids::uuid& environment_id,
                                     const boost::uuids::uuid& tag_id);
    std::vector<domain::tag> get_tags_for_environment(
        const boost::uuids::uuid& environment_id);
    std::vector<domain::environment> get_environments_with_tag(
        const boost::uuids::uuid& tag_id);

    // Connection operations (credentials, optional link to environment)
    void create_connection(domain::connection conn, const std::string& password);
    void update_connection(domain::connection conn,
                           const std::optional<std::string>& password);
    void delete_connection(const boost::uuids::uuid& id);
    std::vector<domain::connection> get_all_connections();
    std::optional<domain::connection> get_connection(const boost::uuids::uuid& id);
    std::vector<domain::connection> get_connections_in_folder(
        const std::optional<boost::uuids::uuid>& folder_id);

    /**
     * @brief Get decrypted password for a connection.
     */
    std::string get_password(const boost::uuids::uuid& connection_id);

    // Connection-tag operations
    void add_tag_to_connection(const boost::uuids::uuid& connection_id,
                               const boost::uuids::uuid& tag_id);
    void remove_tag_from_connection(const boost::uuids::uuid& connection_id,
                                    const boost::uuids::uuid& tag_id);
    std::vector<domain::tag> get_tags_for_connection(
        const boost::uuids::uuid& connection_id);
    std::vector<domain::connection> get_connections_with_tag(
        const boost::uuids::uuid& tag_id);

    /**
     * @brief Resolve a connection to its full details for opening a login dialog.
     *
     * If the connection has an environment_id, fetches host/port from the
     * environment. Decrypts the password. Throws if connection not found.
     */
    resolved_connection resolve_connection(const boost::uuids::uuid& connection_id);

    /**
     * @brief Verify if the master password is correct.
     *
     * Attempts to decrypt an existing password to verify the master password.
     * Returns true if no passwords exist or if decryption succeeds.
     */
    bool verify_master_password();

    /**
     * @brief Change the master password.
     *
     * Re-encrypts all stored connection passwords with the new master password.
     */
    void change_master_password(const std::string& new_password);

    /**
     * @brief Delete all data from the database.
     *
     * Removes all connections, environments, folders, and tags. This operation
     * cannot be undone.
     */
    void purge();

private:
    repository::sqlite_context ctx_;
    repository::folder_repository folder_repo_;
    repository::tag_repository tag_repo_;
    repository::connection_repository conn_repo_;
    repository::connection_tag_repository conn_tag_repo_;
    repository::environment_repository env_repo_;
    repository::environment_tag_repository env_tag_repo_;
    std::string master_password_;
};

}

#endif
