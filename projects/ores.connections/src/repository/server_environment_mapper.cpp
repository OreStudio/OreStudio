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
#include "ores.connections/repository/server_environment_mapper.hpp"

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::connections::repository {

server_environment_entity server_environment_mapper::to_entity(
    const domain::server_environment& env) {
    server_environment_entity e;
    e.id = boost::uuids::to_string(env.id);
    if (env.folder_id) {
        e.folder_id = boost::uuids::to_string(*env.folder_id);
    }
    e.name = env.name;
    e.host = env.host;
    e.port = env.port;
    e.username = env.username;
    e.encrypted_password = env.encrypted_password;
    e.description = env.description;
    return e;
}

domain::server_environment server_environment_mapper::to_domain(
    const server_environment_entity& e) {
    boost::uuids::string_generator gen;
    domain::server_environment env;
    env.id = gen(e.id.value());
    if (e.folder_id) {
        env.folder_id = gen(*e.folder_id);
    }
    env.name = e.name;
    env.host = e.host;
    env.port = e.port;
    env.username = e.username;
    env.encrypted_password = e.encrypted_password;
    env.description = e.description;
    return env;
}

std::vector<server_environment_entity> server_environment_mapper::to_entities(
    const std::vector<domain::server_environment>& envs) {
    std::vector<server_environment_entity> entities;
    entities.reserve(envs.size());
    for (const auto& env : envs) {
        entities.push_back(to_entity(env));
    }
    return entities;
}

std::vector<domain::server_environment> server_environment_mapper::to_domain(
    const std::vector<server_environment_entity>& entities) {
    std::vector<domain::server_environment> envs;
    envs.reserve(entities.size());
    for (const auto& e : entities) {
        envs.push_back(to_domain(e));
    }
    return envs;
}

}
