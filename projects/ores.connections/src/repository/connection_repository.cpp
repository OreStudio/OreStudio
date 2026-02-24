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
#include "ores.connections/repository/connection_repository.hpp"

#include <format>
#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/insert.hpp>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/where.hpp>
#include <sqlgen/literals.hpp>
#include "ores.connections/repository/connection_entity.hpp"
#include "ores.connections/repository/connection_mapper.hpp"

namespace ores::connections::repository {

using namespace sqlgen;
using namespace sqlgen::literals;

connection_repository::connection_repository(sqlite_context& ctx)
    : ctx_(ctx) {}

void connection_repository::write(const domain::connection& conn) {
    auto c = ctx_.connect();
    if (!c) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto entity = connection_mapper::to_entity(conn);
    (*c)->begin_transaction();
    auto result = sqlgen::insert_or_replace(*c, entity);
    if (!result) {
        (*c)->rollback();
        throw std::runtime_error(std::format("Failed to write connection: {}",
            result.error().what()));
    }
    (*c)->commit();
}

void connection_repository::write(const std::vector<domain::connection>& conns) {
    auto c = ctx_.connect();
    if (!c) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto entities = connection_mapper::to_entities(conns);
    (*c)->begin_transaction();
    auto result = sqlgen::insert_or_replace(*c, entities);
    if (!result) {
        (*c)->rollback();
        throw std::runtime_error(std::format("Failed to write connections: {}",
            result.error().what()));
    }
    (*c)->commit();
}

std::vector<domain::connection> connection_repository::read_all() {
    auto c = ctx_.connect();
    if (!c) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto query = sqlgen::read<std::vector<connection_entity>>;
    auto result = query(*c);
    if (!result) {
        throw std::runtime_error(std::format("Failed to read connections: {}",
            result.error().what()));
    }

    return connection_mapper::to_domain(*result);
}

std::optional<domain::connection> connection_repository::read_by_id(
    const boost::uuids::uuid& id) {
    auto c = ctx_.connect();
    if (!c) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string id_str = boost::uuids::to_string(id);
    auto query = sqlgen::read<std::vector<connection_entity>> |
        where("id"_c == id_str);
    auto result = query(*c);
    if (!result) {
        throw std::runtime_error(std::format("Failed to read connection by ID: {}",
            result.error().what()));
    }

    if (result->empty()) {
        return std::nullopt;
    }
    return connection_mapper::to_domain(result->front());
}

std::vector<domain::connection> connection_repository::read_by_folder(
    const std::optional<boost::uuids::uuid>& folder_id) {
    auto c = ctx_.connect();
    if (!c) {
        throw std::runtime_error("Failed to connect to database");
    }

    std::vector<connection_entity> entities;
    if (folder_id) {
        const std::string fid_str = boost::uuids::to_string(*folder_id);
        auto query = sqlgen::read<std::vector<connection_entity>> |
            where("folder_id"_c == fid_str);
        auto result = query(*c);
        if (!result) {
            throw std::runtime_error(std::format("Failed to read connections by folder: {}",
                result.error().what()));
        }
        entities = *result;
    } else {
        auto query = sqlgen::read<std::vector<connection_entity>> |
            where("folder_id"_c.is_null());
        auto result = query(*c);
        if (!result) {
            throw std::runtime_error(std::format("Failed to read root connections: {}",
                result.error().what()));
        }
        entities = *result;
    }

    return connection_mapper::to_domain(entities);
}

void connection_repository::remove(const boost::uuids::uuid& id) {
    auto c = ctx_.connect();
    if (!c) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string id_str = boost::uuids::to_string(id);
    (*c)->begin_transaction();
    auto query = sqlgen::delete_from<connection_entity> | where("id"_c == id_str);
    auto result = query(*c);
    if (!result) {
        (*c)->rollback();
        throw std::runtime_error(std::format("Failed to delete connection: {}",
            result.error().what()));
    }
    (*c)->commit();
}

}
