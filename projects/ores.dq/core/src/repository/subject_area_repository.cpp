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
#include "ores.dq.core/repository/subject_area_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.dq.api/domain/subject_area_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/subject_area_entity.hpp"
#include "ores.dq.core/repository/subject_area_mapper.hpp"
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string subject_area_repository::sql() {
    return generate_create_table_sql<subject_area_entity>(lg());
}

void subject_area_repository::write(context ctx, const domain::subject_area& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing subject area. " << "name: " << v.name
                               << " domain_name: " << v.domain_name;
    execute_write_query(
        ctx, subject_area_mapper::map(v), lg(), "Writing subject area to database.");
}

void subject_area_repository::write(context ctx, const std::vector<domain::subject_area>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing subject areas. Count: " << v.size();
    execute_write_query(
        ctx, subject_area_mapper::map(v), lg(), "Writing subject areas to database.");
}

std::vector<domain::subject_area> subject_area_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<subject_area_entity>> |
                       where("valid_to"_c == max.value()) | order_by("name"_c, "domain_name"_c);

    return execute_read_query<subject_area_entity, domain::subject_area>(
        ctx,
        query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(),
        "Reading latest subject areas");
}

std::vector<domain::subject_area> subject_area_repository::read_latest(
    context ctx, const std::string& name, const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest subject area. " << "name: " << name
                               << " domain_name: " << domain_name;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query =
        sqlgen::read<std::vector<subject_area_entity>> |
        where("name"_c == name && "domain_name"_c == domain_name && "valid_to"_c == max.value());

    return execute_read_query<subject_area_entity, domain::subject_area>(
        ctx,
        query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(),
        "Reading latest subject area by name.");
}


std::vector<domain::subject_area> subject_area_repository::read_all(
    context ctx, const std::string& name, const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all subject area versions. " << "name: " << name
                               << " domain_name: " << domain_name;
    const auto query = sqlgen::read<std::vector<subject_area_entity>> |
                       where("name"_c == name && "domain_name"_c == domain_name) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<subject_area_entity, domain::subject_area>(
        ctx,
        query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(),
        "Reading all subject area versions by name.");
}

std::optional<domain::subject_area> subject_area_repository::read_at_version(
    context ctx, const std::string& name, const std::string& domain_name, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading subject area at version. " << "name: " << name
                               << " domain_name: " << domain_name << " version: " << version;
    const auto query =
        sqlgen::read<std::vector<subject_area_entity>> |
        where("name"_c == name && "domain_name"_c == domain_name && "version"_c == version) |
        sqlgen::limit(1);

    const auto entities = execute_read_query<subject_area_entity, domain::subject_area>(
        ctx,
        query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(),
        "Reading subject area at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void subject_area_repository::remove(context ctx,
                                     const std::string& name,
                                     const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing subject area. " << "name: " << name
                               << " domain_name: " << domain_name;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<subject_area_entity> |
                       where("tenant_id"_c == tid && "name"_c == name &&
                             "domain_name"_c == domain_name && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing subject area from database.");
}

std::vector<domain::subject_area>
subject_area_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest subject areas with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<subject_area_entity>> |
                       where("valid_to"_c == max.value()) | order_by("name"_c, "domain_name"_c) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<subject_area_entity, domain::subject_area>(
        ctx,
        query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(),
        "Reading latest subject areas with pagination.");
}

std::uint32_t subject_area_repository::get_total_area_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active subject area count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<subject_area_entity>(sqlgen::count().as<"count">()) |
                       where("valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active subject area count: " << count;
    return count;
}

void subject_area_repository::remove(context ctx,
                                     const std::vector<std::string>& names,
                                     const std::vector<std::string>& domain_names) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (domain_names.size() != names.size())
        throw std::invalid_argument(
            "subject_area_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < names.size(); ++i)
        remove(ctx, names[i], domain_names[i]);
}


}
