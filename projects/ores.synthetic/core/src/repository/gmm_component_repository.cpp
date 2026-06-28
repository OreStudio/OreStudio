/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.synthetic.core/repository/gmm_component_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.synthetic.api/domain/gmm_component_json_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.core/repository/gmm_component_entity.hpp"
#include "ores.synthetic.core/repository/gmm_component_mapper.hpp"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <rfl.hpp>
#include <rfl/json.hpp>

namespace ores::synthetic::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

using gmmc_entity = gmm_component_entity;
using gmmc_domain = domain::gmm_component;

std::string gmm_component_repository::sql() {
    return generate_create_table_sql<gmmc_entity>(lg());
}

void gmm_component_repository::write(context ctx, const gmmc_domain& component) {
    BOOST_LOG_SEV(lg(), debug) << "Writing component to database: " << component;
    execute_write_query(
        ctx, gmm_component_mapper::map(component), lg(), "Writing component to database.");
}

void gmm_component_repository::write(context ctx, const std::vector<gmmc_domain>& components) {
    BOOST_LOG_SEV(lg(), debug) << "Writing components to database. Count: " << components.size();
    execute_write_query(
        ctx, gmm_component_mapper::map(components), lg(), "Writing components to database.");
}

std::vector<gmmc_domain> gmm_component_repository::read_latest(context ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<gmmc_entity>> |
                       where("valid_to"_c == max.value()) | order_by("valid_from"_c.desc());

    return execute_read_query<gmmc_entity, gmmc_domain>(
        ctx,
        query,
        [](const auto& entities) { return gmm_component_mapper::map(entities); },
        lg(),
        "Reading latest components");
}

std::vector<gmmc_domain> gmm_component_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest components. id: " << id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<gmmc_entity>> |
                       where("id"_c == id && "valid_to"_c == max.value()) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<gmmc_entity, gmmc_domain>(
        ctx,
        query,
        [](const auto& entities) { return gmm_component_mapper::map(entities); },
        lg(),
        "Reading latest components by id.");
}

std::vector<gmmc_domain>
gmm_component_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest components with offset: " << offset
                               << " and limit: " << limit;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<gmmc_entity>> |
                       where("valid_to"_c == max.value()) | order_by("valid_from"_c.desc()) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<gmmc_entity, gmmc_domain>(
        ctx,
        query,
        [](const auto& entities) { return gmm_component_mapper::map(entities); },
        lg(),
        "Reading latest components with pagination.");
}

std::uint32_t gmm_component_repository::get_total_component_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active component count";

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<gmmc_entity>(sqlgen::count().as<"count">()) |
                       where("valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active component count: " << count;
    return count;
}

std::vector<gmmc_domain>
gmm_component_repository::read_at_timepoint(context ctx, const std::string& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading components at timepoint: " << as_of;

    const auto ts = make_timestamp(as_of, lg());
    const auto query = sqlgen::read<std::vector<gmmc_entity>> |
                       where("valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

    return execute_read_query<gmmc_entity, gmmc_domain>(
        ctx,
        query,
        [](const auto& entities) { return gmm_component_mapper::map(entities); },
        lg(),
        "Reading components at timepoint.");
}

std::vector<gmmc_domain>
gmm_component_repository::read_at_timepoint(context ctx,
                                           const std::string& as_of,
                                           const std::string& id) {
    const auto ts = make_timestamp(as_of, lg());
    const auto query = sqlgen::read<std::vector<gmmc_entity>> |
                       where("id"_c == id && "valid_from"_c <= ts.value() &&
                             "valid_to"_c >= ts.value());

    return execute_read_query<gmmc_entity, gmmc_domain>(
        ctx,
        query,
        [](const auto& entities) { return gmm_component_mapper::map(entities); },
        lg(),
        "Reading components at timepoint by id.");
}

std::vector<gmmc_domain> gmm_component_repository::read_all(context ctx) {
    const auto query = sqlgen::read<std::vector<gmmc_entity>> | order_by("valid_from"_c.desc());

    return execute_read_query<gmmc_entity, gmmc_domain>(
        ctx,
        query,
        [](const auto& entities) { return gmm_component_mapper::map(entities); },
        lg(),
        "Reading all components.");
}

std::vector<gmmc_domain> gmm_component_repository::read_all(context ctx, const std::string& id) {
    const auto query = sqlgen::read<std::vector<gmmc_entity>> | where("id"_c == id) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<gmmc_entity, gmmc_domain>(
        ctx,
        query,
        [](const auto& entities) { return gmm_component_mapper::map(entities); },
        lg(),
        "Reading all components by id");
}

void gmm_component_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing component from database: " << id;

    // Delete only the current record - the database trigger will close the
    // temporal record instead of actually deleting it (sets valid_to = current_timestamp)
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::delete_from<gmmc_entity> |
                       where("id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing component from database.");
}

void gmm_component_repository::remove(context ctx, const std::vector<std::string>& ids) {
    const auto query = sqlgen::delete_from<gmmc_entity> | where("id"_c.in(ids));
    execute_delete_query(ctx, query, lg(), "batch removing components");
}

}
