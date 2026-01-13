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

// Server environment operations
void connection_manager::create_environment(domain::server_environment env,
                                             const std::string& password) {
    if (!password.empty()) {
        env.encrypted_password = encryption::encrypt(password, master_password_);
    }
    env_repo_.write(env);
}

void connection_manager::update_environment(domain::server_environment env,
                                             const std::optional<std::string>& password) {
    if (password) {
        if (password->empty()) {
            env.encrypted_password = "";
        } else {
            env.encrypted_password = encryption::encrypt(*password, master_password_);
        }
    } else {
        // Preserve existing encrypted password from database
        auto existing = env_repo_.read_by_id(env.id);
        if (existing) {
            env.encrypted_password = existing->encrypted_password;
        } else {
            throw std::runtime_error("Cannot update non-existent environment");
        }
    }
    env_repo_.write(env);
}

void connection_manager::delete_environment(const boost::uuids::uuid& id) {
    env_repo_.remove(id);
}

std::vector<domain::server_environment> connection_manager::get_all_environments() {
    return env_repo_.read_all();
}

std::optional<domain::server_environment> connection_manager::get_environment(
    const boost::uuids::uuid& id) {
    return env_repo_.read_by_id(id);
}

std::vector<domain::server_environment> connection_manager::get_environments_in_folder(
    const std::optional<boost::uuids::uuid>& folder_id) {
    return env_repo_.read_by_folder(folder_id);
}

std::string connection_manager::get_password(const boost::uuids::uuid& environment_id) {
    auto env = env_repo_.read_by_id(environment_id);
    if (!env) {
        throw std::runtime_error("Environment not found");
    }

    if (env->encrypted_password.empty()) {
        return "";
    }

    return encryption::decrypt(env->encrypted_password, master_password_);
}

// Environment-tag operations
void connection_manager::add_tag_to_environment(const boost::uuids::uuid& environment_id,
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

std::vector<domain::server_environment> connection_manager::get_environments_with_tag(
    const boost::uuids::uuid& tag_id) {
    auto associations = env_tag_repo_.read_by_tag(tag_id);
    std::vector<domain::server_environment> envs;
    envs.reserve(associations.size());

    for (const auto& assoc : associations) {
        auto env = env_repo_.read_by_id(assoc.environment_id);
        if (env) {
            envs.push_back(*env);
        }
    }

    return envs;
}

bool connection_manager::verify_master_password() {
    auto envs = env_repo_.read_all();
    for (const auto& env : envs) {
        if (!env.encrypted_password.empty()) {
            return encryption::verify_password(env.encrypted_password,
                                                        master_password_);
        }
    }
    return true;  // No passwords to verify
}

void connection_manager::change_master_password(const std::string& new_password) {
    auto envs = env_repo_.read_all();

    for (auto& env : envs) {
        if (!env.encrypted_password.empty()) {
            // Decrypt with old password, encrypt with new
            std::string plaintext = encryption::decrypt(
                env.encrypted_password, master_password_);
            env.encrypted_password = encryption::encrypt(plaintext, new_password);
            env_repo_.write(env);
        }
    }

    master_password_ = new_password;
}

}
