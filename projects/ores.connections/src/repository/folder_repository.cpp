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
#include "ores.connections/repository/folder_repository.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/insert.hpp>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/where.hpp>
#include <sqlgen/literals.hpp>
#include "ores.connections/repository/folder_entity.hpp"
#include "ores.connections/repository/folder_mapper.hpp"

namespace ores::connections::repository {

using namespace sqlgen;
using namespace sqlgen::literals;

folder_repository::folder_repository(sqlite_context& ctx)
    : ctx_(ctx) {}

void folder_repository::write(const domain::folder& f) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto entity = folder_mapper::to_entity(f);
    (*conn)->begin_transaction();
    auto result = sqlgen::insert_or_replace(*conn, entity);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error("Failed to write folder");
    }
    (*conn)->commit();
}

void folder_repository::write(const std::vector<domain::folder>& folders) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto entities = folder_mapper::to_entities(folders);
    (*conn)->begin_transaction();
    auto result = sqlgen::insert_or_replace(*conn, entities);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error("Failed to write folders");
    }
    (*conn)->commit();
}

std::vector<domain::folder> folder_repository::read_all() {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto query = sqlgen::read<std::vector<folder_entity>>;
    auto result = query(*conn);
    if (!result) {
        throw std::runtime_error("Failed to read folders");
    }

    return folder_mapper::to_domain(*result);
}

std::optional<domain::folder> folder_repository::read_by_id(
    const boost::uuids::uuid& id) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string id_str = boost::uuids::to_string(id);
    auto query = sqlgen::read<std::vector<folder_entity>> |
        where("id"_c == id_str);
    auto result = query(*conn);
    if (!result) {
        throw std::runtime_error("Failed to read folder by ID");
    }

    if (result->empty()) {
        return std::nullopt;
    }
    return folder_mapper::to_domain(result->front());
}

std::vector<domain::folder> folder_repository::read_by_parent(
    const std::optional<boost::uuids::uuid>& parent_id) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    std::vector<folder_entity> entities;
    if (parent_id) {
        const std::string pid_str = boost::uuids::to_string(*parent_id);
        auto query = sqlgen::read<std::vector<folder_entity>> |
            where("parent_id"_c == pid_str);
        auto result = query(*conn);
        if (!result) {
            throw std::runtime_error("Failed to read folders by parent");
        }
        entities = *result;
    } else {
        auto query = sqlgen::read<std::vector<folder_entity>> |
            where("parent_id"_c.is_null());
        auto result = query(*conn);
        if (!result) {
            throw std::runtime_error("Failed to read root folders");
        }
        entities = *result;
    }

    return folder_mapper::to_domain(entities);
}

void folder_repository::remove(const boost::uuids::uuid& id) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string id_str = boost::uuids::to_string(id);
    (*conn)->begin_transaction();
    auto query = sqlgen::delete_from<folder_entity> | where("id"_c == id_str);
    auto result = query(*conn);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error("Failed to delete folder");
    }
    (*conn)->commit();
}

}
