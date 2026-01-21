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
#include "ores.connections/repository/server_environment_repository.hpp"

#include <format>
#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/insert.hpp>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/where.hpp>
#include <sqlgen/literals.hpp>
#include "ores.connections/repository/server_environment_entity.hpp"
#include "ores.connections/repository/server_environment_mapper.hpp"

namespace ores::connections::repository {

using namespace sqlgen;
using namespace sqlgen::literals;

server_environment_repository::server_environment_repository(sqlite_context& ctx)
    : ctx_(ctx) {}

void server_environment_repository::write(const domain::server_environment& env) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto entity = server_environment_mapper::to_entity(env);
    (*conn)->begin_transaction();
    auto result = sqlgen::insert_or_replace(*conn, entity);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error(std::format("Failed to write server environment: {}",
            result.error().what()));
    }
    (*conn)->commit();
}

void server_environment_repository::write(
    const std::vector<domain::server_environment>& envs) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto entities = server_environment_mapper::to_entities(envs);
    (*conn)->begin_transaction();
    auto result = sqlgen::insert_or_replace(*conn, entities);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error(std::format("Failed to write server environments: {}",
            result.error().what()));
    }
    (*conn)->commit();
}

std::vector<domain::server_environment> server_environment_repository::read_all() {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto query = sqlgen::read<std::vector<server_environment_entity>>;
    auto result = query(*conn);
    if (!result) {
        throw std::runtime_error(std::format("Failed to read server environments: {}",
            result.error().what()));
    }

    return server_environment_mapper::to_domain(*result);
}

std::optional<domain::server_environment> server_environment_repository::read_by_id(
    const boost::uuids::uuid& id) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string id_str = boost::uuids::to_string(id);
    auto query = sqlgen::read<std::vector<server_environment_entity>> |
        where("id"_c == id_str);
    auto result = query(*conn);
    if (!result) {
        throw std::runtime_error(std::format("Failed to read server environment by ID: {}",
            result.error().what()));
    }

    if (result->empty()) {
        return std::nullopt;
    }
    return server_environment_mapper::to_domain(result->front());
}

std::vector<domain::server_environment> server_environment_repository::read_by_folder(
    const std::optional<boost::uuids::uuid>& folder_id) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    std::vector<server_environment_entity> entities;
    if (folder_id) {
        const std::string fid_str = boost::uuids::to_string(*folder_id);
        auto query = sqlgen::read<std::vector<server_environment_entity>> |
            where("folder_id"_c == fid_str);
        auto result = query(*conn);
        if (!result) {
            throw std::runtime_error(std::format("Failed to read server environments by folder: {}",
                result.error().what()));
        }
        entities = *result;
    } else {
        auto query = sqlgen::read<std::vector<server_environment_entity>> |
            where("folder_id"_c.is_null());
        auto result = query(*conn);
        if (!result) {
            throw std::runtime_error(std::format("Failed to read root server environments: {}",
                result.error().what()));
        }
        entities = *result;
    }

    return server_environment_mapper::to_domain(entities);
}

void server_environment_repository::remove(const boost::uuids::uuid& id) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string id_str = boost::uuids::to_string(id);
    (*conn)->begin_transaction();
    auto query = sqlgen::delete_from<server_environment_entity> | where("id"_c == id_str);
    auto result = query(*conn);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error(std::format("Failed to delete server environment: {}",
            result.error().what()));
    }
    (*conn)->commit();
}

}
