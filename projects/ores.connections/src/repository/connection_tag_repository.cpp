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
#include "ores.connections/repository/connection_tag_repository.hpp"

#include <format>
#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/insert.hpp>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/where.hpp>
#include <sqlgen/literals.hpp>
#include "ores.connections/repository/connection_tag_entity.hpp"
#include "ores.connections/repository/connection_tag_mapper.hpp"

namespace ores::connections::repository {

using namespace sqlgen;
using namespace sqlgen::literals;

connection_tag_repository::connection_tag_repository(sqlite_context& ctx)
    : ctx_(ctx) {}

void connection_tag_repository::write(const domain::connection_tag& ct) {
    auto c = ctx_.connect();
    if (!c) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto entity = connection_tag_mapper::to_entity(ct);
    (*c)->begin_transaction();
    auto result = sqlgen::insert(*c, entity);
    if (!result) {
        (*c)->rollback();
        throw std::runtime_error(std::format("Failed to write connection tag: {}",
            result.error().what()));
    }
    (*c)->commit();
}

void connection_tag_repository::write(
    const std::vector<domain::connection_tag>& tags) {
    auto c = ctx_.connect();
    if (!c) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto entities = connection_tag_mapper::to_entities(tags);
    (*c)->begin_transaction();
    auto result = sqlgen::insert(*c, entities);
    if (!result) {
        (*c)->rollback();
        throw std::runtime_error(std::format("Failed to write connection tags: {}",
            result.error().what()));
    }
    (*c)->commit();
}

std::vector<domain::connection_tag> connection_tag_repository::read_by_connection(
    const boost::uuids::uuid& connection_id) {
    auto c = ctx_.connect();
    if (!c) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string cid_str = boost::uuids::to_string(connection_id);
    auto query = sqlgen::read<std::vector<connection_tag_entity>> |
        where("connection_id"_c == cid_str);
    auto result = query(*c);
    if (!result) {
        throw std::runtime_error(std::format("Failed to read connection tags: {}",
            result.error().what()));
    }

    return connection_tag_mapper::to_domain(*result);
}

std::vector<domain::connection_tag> connection_tag_repository::read_by_tag(
    const boost::uuids::uuid& tag_id) {
    auto c = ctx_.connect();
    if (!c) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string tid_str = boost::uuids::to_string(tag_id);
    auto query = sqlgen::read<std::vector<connection_tag_entity>> |
        where("tag_id"_c == tid_str);
    auto result = query(*c);
    if (!result) {
        throw std::runtime_error(std::format("Failed to read connection tags by tag: {}",
            result.error().what()));
    }

    return connection_tag_mapper::to_domain(*result);
}

void connection_tag_repository::remove(
    const boost::uuids::uuid& connection_id, const boost::uuids::uuid& tag_id) {
    auto c = ctx_.connect();
    if (!c) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string cid_str = boost::uuids::to_string(connection_id);
    const std::string tid_str = boost::uuids::to_string(tag_id);
    (*c)->begin_transaction();
    auto query = sqlgen::delete_from<connection_tag_entity> |
        where("connection_id"_c == cid_str && "tag_id"_c == tid_str);
    auto result = query(*c);
    if (!result) {
        (*c)->rollback();
        throw std::runtime_error(std::format("Failed to delete connection tag: {}",
            result.error().what()));
    }
    (*c)->commit();
}

void connection_tag_repository::remove_by_connection(
    const boost::uuids::uuid& connection_id) {
    auto c = ctx_.connect();
    if (!c) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string cid_str = boost::uuids::to_string(connection_id);
    (*c)->begin_transaction();
    auto query = sqlgen::delete_from<connection_tag_entity> |
        where("connection_id"_c == cid_str);
    auto result = query(*c);
    if (!result) {
        (*c)->rollback();
        throw std::runtime_error(std::format("Failed to delete connection tags: {}",
            result.error().what()));
    }
    (*c)->commit();
}

void connection_tag_repository::remove_by_tag(const boost::uuids::uuid& tag_id) {
    auto c = ctx_.connect();
    if (!c) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string tid_str = boost::uuids::to_string(tag_id);
    (*c)->begin_transaction();
    auto query = sqlgen::delete_from<connection_tag_entity> |
        where("tag_id"_c == tid_str);
    auto result = query(*c);
    if (!result) {
        (*c)->rollback();
        throw std::runtime_error(std::format("Failed to delete connection tags by tag: {}",
            result.error().what()));
    }
    (*c)->commit();
}

}
