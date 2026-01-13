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
#include "ores.connections/repository/environment_tag_repository.hpp"

#include <format>
#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/insert.hpp>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/where.hpp>
#include <sqlgen/literals.hpp>
#include "ores.connections/repository/environment_tag_entity.hpp"
#include "ores.connections/repository/environment_tag_mapper.hpp"

namespace ores::connections::repository {

using namespace sqlgen;
using namespace sqlgen::literals;

environment_tag_repository::environment_tag_repository(sqlite_context& ctx)
    : ctx_(ctx) {}

void environment_tag_repository::write(const domain::environment_tag& et) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto entity = environment_tag_mapper::to_entity(et);
    (*conn)->begin_transaction();
    auto result = sqlgen::insert(*conn, entity);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error(std::format("Failed to write environment tag: {}",
            result.error().what()));
    }
    (*conn)->commit();
}

void environment_tag_repository::write(
    const std::vector<domain::environment_tag>& tags) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    auto entities = environment_tag_mapper::to_entities(tags);
    (*conn)->begin_transaction();
    auto result = sqlgen::insert(*conn, entities);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error(std::format("Failed to write environment tags: {}",
            result.error().what()));
    }
    (*conn)->commit();
}

std::vector<domain::environment_tag> environment_tag_repository::read_by_environment(
    const boost::uuids::uuid& environment_id) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string eid_str = boost::uuids::to_string(environment_id);
    auto query = sqlgen::read<std::vector<environment_tag_entity>> |
        where("environment_id"_c == eid_str);
    auto result = query(*conn);
    if (!result) {
        throw std::runtime_error(std::format("Failed to read environment tags: {}",
            result.error().what()));
    }

    return environment_tag_mapper::to_domain(*result);
}

std::vector<domain::environment_tag> environment_tag_repository::read_by_tag(
    const boost::uuids::uuid& tag_id) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string tid_str = boost::uuids::to_string(tag_id);
    auto query = sqlgen::read<std::vector<environment_tag_entity>> |
        where("tag_id"_c == tid_str);
    auto result = query(*conn);
    if (!result) {
        throw std::runtime_error(std::format("Failed to read environment tags by tag: {}",
            result.error().what()));
    }

    return environment_tag_mapper::to_domain(*result);
}

void environment_tag_repository::remove(
    const boost::uuids::uuid& environment_id, const boost::uuids::uuid& tag_id) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string eid_str = boost::uuids::to_string(environment_id);
    const std::string tid_str = boost::uuids::to_string(tag_id);
    (*conn)->begin_transaction();
    auto query = sqlgen::delete_from<environment_tag_entity> |
        where("environment_id"_c == eid_str && "tag_id"_c == tid_str);
    auto result = query(*conn);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error(std::format("Failed to delete environment tag: {}",
            result.error().what()));
    }
    (*conn)->commit();
}

void environment_tag_repository::remove_by_environment(
    const boost::uuids::uuid& environment_id) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string eid_str = boost::uuids::to_string(environment_id);
    (*conn)->begin_transaction();
    auto query = sqlgen::delete_from<environment_tag_entity> |
        where("environment_id"_c == eid_str);
    auto result = query(*conn);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error(std::format("Failed to delete environment tags: {}",
            result.error().what()));
    }
    (*conn)->commit();
}

void environment_tag_repository::remove_by_tag(const boost::uuids::uuid& tag_id) {
    auto conn = ctx_.connect();
    if (!conn) {
        throw std::runtime_error("Failed to connect to database");
    }

    const std::string tid_str = boost::uuids::to_string(tag_id);
    (*conn)->begin_transaction();
    auto query = sqlgen::delete_from<environment_tag_entity> |
        where("tag_id"_c == tid_str);
    auto result = query(*conn);
    if (!result) {
        (*conn)->rollback();
        throw std::runtime_error(std::format("Failed to delete environment tags by tag: {}",
            result.error().what()));
    }
    (*conn)->commit();
}

}
