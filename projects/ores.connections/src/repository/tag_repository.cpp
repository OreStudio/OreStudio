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
#include "ores.connections/repository/tag_repository.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/insert.hpp>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/where.hpp>
#include <sqlgen/literals.hpp>
#include "ores.connections/repository/tag_entity.hpp"
#include "ores.connections/repository/tag_mapper.hpp"

namespace ores::connections::repository {

using namespace sqlgen;
using namespace sqlgen::literals;

tag_repository::tag_repository(sqlite_context& ctx)
    : ctx_(ctx) {}

void tag_repository::write(const domain::tag& t) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto entity = tag_mapper::to_entity(t);
    (*conn)->begin_transaction();
    auto result = sqlgen::insert_or_replace(*conn, entity);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error("Failed to write tag");
    }
    (*conn)->commit();
}

void tag_repository::write(const std::vector<domain::tag>& tags) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto entities = tag_mapper::to_entities(tags);
    (*conn)->begin_transaction();
    auto result = sqlgen::insert_or_replace(*conn, entities);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error("Failed to write tags");
    }
    (*conn)->commit();
}

std::vector<domain::tag> tag_repository::read_all() {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto query = sqlgen::read<std::vector<tag_entity>>;
    auto result = query(*conn);
    if (!result) {
        throw std::runtime_error("Failed to read tags");
    }

    return tag_mapper::to_domain(*result);
}

std::optional<domain::tag> tag_repository::read_by_id(
    const boost::uuids::uuid& id) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string id_str = boost::uuids::to_string(id);
    auto query = sqlgen::read<std::vector<tag_entity>> |
        where("id"_c == id_str);
    auto result = query(*conn);
    if (!result) {
        throw std::runtime_error("Failed to read tag by ID");
    }

    if (result->empty()) {
        return std::nullopt;
    }
    return tag_mapper::to_domain(result->front());
}

std::optional<domain::tag> tag_repository::read_by_name(const std::string& name) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto query = sqlgen::read<std::vector<tag_entity>> |
        where("name"_c == name);
    auto result = query(*conn);
    if (!result) {
        throw std::runtime_error("Failed to read tag by name");
    }

    if (result->empty()) {
        return std::nullopt;
    }
    return tag_mapper::to_domain(result->front());
}

void tag_repository::remove(const boost::uuids::uuid& id) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string id_str = boost::uuids::to_string(id);
    (*conn)->begin_transaction();
    auto query = sqlgen::delete_from<tag_entity> | where("id"_c == id_str);
    auto result = query(*conn);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error("Failed to delete tag");
    }
    (*conn)->commit();
}

}
