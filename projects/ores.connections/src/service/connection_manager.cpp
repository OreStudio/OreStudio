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
#include "ores.connections/service/connection_manager.hpp"

#include <stdexcept>
#include "ores.security/crypto/encryption.hpp"

namespace ores::connections::service {

using ores::security::crypto::encryption;

connection_manager::connection_manager(const std::filesystem::path& db_path,
                                        const std::string& master_password)
    : ctx_(db_path),
      folder_repo_(ctx_),
      tag_repo_(ctx_),
      conn_repo_(ctx_),
      conn_tag_repo_(ctx_),
      env_repo_(ctx_),
      env_tag_repo_(ctx_),
      master_password_(master_password) {
    ctx_.initialize_schema();
}

// Folder operations
void connection_manager::create_folder(const domain::folder& folder) {
    folder_repo_.write(folder);
}

void connection_manager::update_folder(const domain::folder& folder) {
    folder_repo_.write(folder);
}

void connection_manager::delete_folder(const boost::uuids::uuid& id) {
    folder_repo_.remove(id);
}

std::vector<domain::folder> connection_manager::get_all_folders() {
    return folder_repo_.read_all();
}

std::optional<domain::folder> connection_manager::get_folder(
    const boost::uuids::uuid& id) {
    return folder_repo_.read_by_id(id);
}

std::vector<domain::folder> connection_manager::get_root_folders() {
    return folder_repo_.read_by_parent(std::nullopt);
}

std::vector<domain::folder> connection_manager::get_child_folders(
    const boost::uuids::uuid& parent_id) {
    return folder_repo_.read_by_parent(parent_id);
}

// Tag operations
void connection_manager::create_tag(const domain::tag& tag) {
    tag_repo_.write(tag);
}

void connection_manager::update_tag(const domain::tag& tag) {
    tag_repo_.write(tag);
}

void connection_manager::delete_tag(const boost::uuids::uuid& id) {
    tag_repo_.remove(id);
}

std::vector<domain::tag> connection_manager::get_all_tags() {
    return tag_repo_.read_all();
}

std::optional<domain::tag> connection_manager::get_tag(const boost::uuids::uuid& id) {
    return tag_repo_.read_by_id(id);
}

std::optional<domain::tag> connection_manager::get_tag_by_name(const std::string& name) {
    return tag_repo_.read_by_name(name);
}

// Environment operations (pure host/port, no credentials)
void connection_manager::create_environment(const domain::environment& env) {
    env_repo_.write(env);
}

void connection_manager::update_environment(const domain::environment& env) {
    env_repo_.write(env);
}

void connection_manager::delete_environment(const boost::uuids::uuid& id) {
    env_repo_.remove(id);
}

std::vector<domain::environment> connection_manager::get_all_environments() {
    return env_repo_.read_all();
}

std::optional<domain::environment> connection_manager::get_environment(
    const boost::uuids::uuid& id) {
    return env_repo_.read_by_id(id);
}

std::vector<domain::environment> connection_manager::get_environments_in_folder(
    const std::optional<boost::uuids::uuid>& folder_id) {
    return env_repo_.read_by_folder(folder_id);
}

// Environment-tag operations
void connection_manager::add_tag_to_environment(
    const boost::uuids::uuid& environment_id,
    const boost::uuids::uuid& tag_id) {
    domain::environment_tag et;
    et.environment_id = environment_id;
    et.tag_id = tag_id;
    env_tag_repo_.write(et);
}

void connection_manager::remove_tag_from_environment(
    const boost::uuids::uuid& environment_id,
    const boost::uuids::uuid& tag_id) {
    env_tag_repo_.remove(environment_id, tag_id);
}

std::vector<domain::tag> connection_manager::get_tags_for_environment(
    const boost::uuids::uuid& environment_id) {
    auto associations = env_tag_repo_.read_by_environment(environment_id);
    std::vector<domain::tag> tags;
    tags.reserve(associations.size());

    for (const auto& assoc : associations) {
        auto tag = tag_repo_.read_by_id(assoc.tag_id);
        if (tag) {
            tags.push_back(*tag);
        }
    }

    return tags;
}

std::vector<domain::environment> connection_manager::get_environments_with_tag(
    const boost::uuids::uuid& tag_id) {
    auto associations = env_tag_repo_.read_by_tag(tag_id);
    std::vector<domain::environment> envs;
    envs.reserve(associations.size());

    for (const auto& assoc : associations) {
        auto env = env_repo_.read_by_id(assoc.environment_id);
        if (env) {
            envs.push_back(*env);
        }
    }

    return envs;
}

// Connection operations (credentials, optional link to environment)
void connection_manager::create_connection(domain::connection conn,
                                            const std::string& password) {
    if (!password.empty()) {
        conn.encrypted_password = encryption::encrypt(password, master_password_);
    }
    conn_repo_.write(conn);
}

void connection_manager::update_connection(domain::connection conn,
                                            const std::optional<std::string>& password) {
    if (password) {
        if (password->empty()) {
            conn.encrypted_password = "";
        } else {
            conn.encrypted_password = encryption::encrypt(*password, master_password_);
        }
    } else {
        // Preserve existing encrypted password from database
        auto existing = conn_repo_.read_by_id(conn.id);
        if (existing) {
            conn.encrypted_password = existing->encrypted_password;
        } else {
            throw std::runtime_error("Cannot update non-existent connection");
        }
    }
    conn_repo_.write(conn);
}

void connection_manager::delete_connection(const boost::uuids::uuid& id) {
    conn_repo_.remove(id);
}

std::vector<domain::connection> connection_manager::get_all_connections() {
    return conn_repo_.read_all();
}

std::optional<domain::connection> connection_manager::get_connection(
    const boost::uuids::uuid& id) {
    return conn_repo_.read_by_id(id);
}

std::vector<domain::connection> connection_manager::get_connections_in_folder(
    const std::optional<boost::uuids::uuid>& folder_id) {
    return conn_repo_.read_by_folder(folder_id);
}

std::string connection_manager::get_password(const boost::uuids::uuid& connection_id) {
    auto conn = conn_repo_.read_by_id(connection_id);
    if (!conn) {
        throw std::runtime_error("Connection not found");
    }

    if (conn->encrypted_password.empty()) {
        return "";
    }

    return encryption::decrypt(conn->encrypted_password, master_password_);
}

// Connection-tag operations
void connection_manager::add_tag_to_connection(
    const boost::uuids::uuid& connection_id,
    const boost::uuids::uuid& tag_id) {
    domain::connection_tag ct;
    ct.connection_id = connection_id;
    ct.tag_id = tag_id;
    conn_tag_repo_.write(ct);
}

void connection_manager::remove_tag_from_connection(
    const boost::uuids::uuid& connection_id,
    const boost::uuids::uuid& tag_id) {
    conn_tag_repo_.remove(connection_id, tag_id);
}

std::vector<domain::tag> connection_manager::get_tags_for_connection(
    const boost::uuids::uuid& connection_id) {
    auto associations = conn_tag_repo_.read_by_connection(connection_id);
    std::vector<domain::tag> tags;
    tags.reserve(associations.size());

    for (const auto& assoc : associations) {
        auto tag = tag_repo_.read_by_id(assoc.tag_id);
        if (tag) {
            tags.push_back(*tag);
        }
    }

    return tags;
}

std::vector<domain::connection> connection_manager::get_connections_with_tag(
    const boost::uuids::uuid& tag_id) {
    auto associations = conn_tag_repo_.read_by_tag(tag_id);
    std::vector<domain::connection> conns;
    conns.reserve(associations.size());

    for (const auto& assoc : associations) {
        auto conn = conn_repo_.read_by_id(assoc.connection_id);
        if (conn) {
            conns.push_back(*conn);
        }
    }

    return conns;
}

connection_manager::resolved_connection connection_manager::resolve_connection(
    const boost::uuids::uuid& connection_id) {
    auto conn = conn_repo_.read_by_id(connection_id);
    if (!conn) {
        throw std::runtime_error("Connection not found");
    }

    resolved_connection result;
    result.name = conn->name;
    result.username = conn->username;
    result.environment_id = conn->environment_id;

    if (conn->environment_id) {
        // Resolve host/port from linked environment
        auto env = env_repo_.read_by_id(*conn->environment_id);
        if (!env) {
            throw std::runtime_error("Linked environment not found");
        }
        result.host = env->host;
        result.port = env->port;
        result.environment_name = env->name;
    } else {
        // Use host/port stored directly on the connection
        result.host = conn->host.value_or("");
        result.port = conn->port.value_or(0);
    }

    if (!conn->encrypted_password.empty()) {
        result.password = encryption::decrypt(conn->encrypted_password, master_password_);
    }

    return result;
}

bool connection_manager::verify_master_password() {
    auto conns = conn_repo_.read_all();
    for (const auto& conn : conns) {
        if (!conn.encrypted_password.empty()) {
            return encryption::verify_password(conn.encrypted_password,
                                               master_password_);
        }
    }
    return true;  // No passwords to verify
}

void connection_manager::change_master_password(const std::string& new_password) {
    auto conns = conn_repo_.read_all();

    for (auto& conn : conns) {
        if (!conn.encrypted_password.empty()) {
            // Decrypt with old password, encrypt with new
            std::string plaintext = encryption::decrypt(
                conn.encrypted_password, master_password_);
            conn.encrypted_password = encryption::encrypt(plaintext, new_password);
            conn_repo_.write(conn);
        }
    }

    master_password_ = new_password;
}

void connection_manager::purge() {
    ctx_.purge_all_data();
}

}
