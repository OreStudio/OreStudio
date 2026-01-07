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
#include "ores.assets/repository/tag_repository.hpp"

#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.assets/repository/tag_mapper.hpp"
#include "ores.assets/repository/tag_entity.hpp"

namespace ores::assets::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::telemetry::log;
using namespace ores::database::repository;

std::string tag_repository::sql() {
    return generate_create_table_sql<tag_entity>(lg());
}

void tag_repository::write(context ctx, const domain::tag& tag) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tag to database. Name: " << tag.name;

    execute_write_query(ctx, tag_mapper::map(tag),
        lg(), "Writing tag to database.");
}

void tag_repository::write(context ctx, const std::vector<domain::tag>& tags) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tags to database. Count: "
                               << tags.size();

    execute_write_query(ctx, tag_mapper::map(tags),
        lg(), "Writing tags to database.");
}

std::vector<domain::tag> tag_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tag_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<tag_entity, domain::tag>(ctx, query,
        [](const auto& entities) { return tag_mapper::map(entities); },
        lg(), "Reading latest tags");
}

std::vector<domain::tag>
tag_repository::read_latest_by_id(context ctx, const std::string& tag_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tags by ID: " << tag_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tag_entity>> |
        where("tag_id"_c == tag_id && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<tag_entity, domain::tag>(ctx, query,
        [](const auto& entities) { return tag_mapper::map(entities); },
        lg(), "Reading latest tags by ID.");
}

std::vector<domain::tag>
tag_repository::read_latest_by_name(context ctx, const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tags by name: " << name;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tag_entity>> |
        where("name"_c == name && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<tag_entity, domain::tag>(ctx, query,
        [](const auto& entities) { return tag_mapper::map(entities); },
        lg(), "Reading latest tags by name.");
}

std::vector<domain::tag>
tag_repository::read_latest(context ctx, std::uint32_t offset,
                            std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tags with offset: "
                               << offset << " and limit: " << limit;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tag_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc()) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<tag_entity, domain::tag>(ctx, query,
        [](const auto& entities) { return tag_mapper::map(entities); },
        lg(), "Reading latest tags with pagination.");
}

std::uint32_t tag_repository::get_total_tag_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active tag count";

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<tag_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active tag count: " << count;
    return count;
}

std::vector<domain::tag> tag_repository::read_all(context ctx) {
    const auto query = sqlgen::read<std::vector<tag_entity>> |
        order_by("valid_from"_c.desc());

    return execute_read_query<tag_entity, domain::tag>(ctx, query,
        [](const auto& entities) { return tag_mapper::map(entities); },
        lg(), "Reading all tags.");
}

void tag_repository::remove(context ctx, const std::string& tag_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tag from database: " << tag_id;

    const auto query = sqlgen::delete_from<tag_entity> |
        where("tag_id"_c == tag_id);

    execute_delete_query(ctx, query, lg(), "Removing tag from database.");
}

}
