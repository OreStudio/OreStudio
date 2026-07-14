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
#include "ores.synthetic.core/repository/ir_curve_template_entry_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.synthetic.api/domain/ir_curve_template_entry_json_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.core/repository/ir_curve_template_entry_entity.hpp"
#include "ores.synthetic.core/repository/ir_curve_template_entry_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::synthetic::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string ir_curve_template_entry_repository::sql() {
    return generate_create_table_sql<ir_curve_template_entry_entity>(lg());
}

void ir_curve_template_entry_repository::write(context ctx,
                                               const domain::ir_curve_template_entry& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing IR curve template entry: " << v.id;
    execute_write_query(ctx,
                        ir_curve_template_entry_mapper::map(v),
                        lg(),
                        "Writing IR curve template entry to database.");
}

void ir_curve_template_entry_repository::write(
    context ctx, const std::vector<domain::ir_curve_template_entry>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing IR curve template entries. Count: " << v.size();
    execute_write_query(ctx,
                        ir_curve_template_entry_mapper::map(v),
                        lg(),
                        "Writing IR curve template entries to database.");
}

std::vector<domain::ir_curve_template_entry>
ir_curve_template_entry_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<ir_curve_template_entry_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<ir_curve_template_entry_entity, domain::ir_curve_template_entry>(
        ctx,
        query,
        [](const auto& entities) { return ir_curve_template_entry_mapper::map(entities); },
        lg(),
        "Reading latest IR curve template entries");
}

std::vector<domain::ir_curve_template_entry>
ir_curve_template_entry_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest IR curve template entry. id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<ir_curve_template_entry_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<ir_curve_template_entry_entity, domain::ir_curve_template_entry>(
        ctx,
        query,
        [](const auto& entities) { return ir_curve_template_entry_mapper::map(entities); },
        lg(),
        "Reading latest IR curve template entry by id.");
}

std::vector<domain::ir_curve_template_entry>
ir_curve_template_entry_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all IR curve template entry versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<ir_curve_template_entry_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<ir_curve_template_entry_entity, domain::ir_curve_template_entry>(
        ctx,
        query,
        [](const auto& entities) { return ir_curve_template_entry_mapper::map(entities); },
        lg(),
        "Reading all IR curve template entry versions by id.");
}

std::optional<domain::ir_curve_template_entry> ir_curve_template_entry_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading IR curve template entry at version. id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<ir_curve_template_entry_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<ir_curve_template_entry_entity, domain::ir_curve_template_entry>(
            ctx,
            query,
            [](const auto& entities) { return ir_curve_template_entry_mapper::map(entities); },
            lg(),
            "Reading IR curve template entry at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


void ir_curve_template_entry_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing IR curve template entry: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<ir_curve_template_entry_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing IR curve template entry from database.");
}

std::vector<domain::ir_curve_template_entry> ir_curve_template_entry_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest IR curve template entries with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<ir_curve_template_entry_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<ir_curve_template_entry_entity, domain::ir_curve_template_entry>(
        ctx,
        query,
        [](const auto& entities) { return ir_curve_template_entry_mapper::map(entities); },
        lg(),
        "Reading latest IR curve template entries with pagination.");
}

std::uint32_t
ir_curve_template_entry_repository::get_total_ir_curve_template_entry_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active IR curve template entry count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<ir_curve_template_entry_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active IR curve template entry count: " << count;
    return count;
}

void ir_curve_template_entry_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<ir_curve_template_entry_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing IR curve template entries.");
}


}
