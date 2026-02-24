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
#include "ores.connections/repository/connection_mapper.hpp"

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::connections::repository {

connection_entity connection_mapper::to_entity(const domain::connection& conn) {
    connection_entity e;
    e.id = boost::uuids::to_string(conn.id);
    if (conn.folder_id) {
        e.folder_id = boost::uuids::to_string(*conn.folder_id);
    }
    if (conn.environment_id) {
        e.environment_id = boost::uuids::to_string(*conn.environment_id);
    }
    e.name = conn.name;
    e.host = conn.host;
    e.port = conn.port;
    e.username = conn.username;
    e.encrypted_password = conn.encrypted_password;
    e.description = conn.description;
    return e;
}

domain::connection connection_mapper::to_domain(const connection_entity& e) {
    boost::uuids::string_generator gen;
    domain::connection conn;
    conn.id = gen(e.id.value());
    if (e.folder_id) {
        conn.folder_id = gen(*e.folder_id);
    }
    if (e.environment_id) {
        conn.environment_id = gen(*e.environment_id);
    }
    conn.name = e.name;
    conn.host = e.host;
    conn.port = e.port;
    conn.username = e.username;
    conn.encrypted_password = e.encrypted_password;
    conn.description = e.description;
    return conn;
}

std::vector<connection_entity> connection_mapper::to_entities(
    const std::vector<domain::connection>& conns) {
    std::vector<connection_entity> entities;
    entities.reserve(conns.size());
    for (const auto& conn : conns) {
        entities.push_back(to_entity(conn));
    }
    return entities;
}

std::vector<domain::connection> connection_mapper::to_domain(
    const std::vector<connection_entity>& entities) {
    std::vector<domain::connection> conns;
    conns.reserve(entities.size());
    for (const auto& e : entities) {
        conns.push_back(to_domain(e));
    }
    return conns;
}

}
