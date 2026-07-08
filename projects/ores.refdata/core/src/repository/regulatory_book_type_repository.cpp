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
#include "ores.refdata.core/repository/regulatory_book_type_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/regulatory_book_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/regulatory_book_type_entity.hpp"
#include "ores.refdata.core/repository/regulatory_book_type_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string regulatory_book_type_repository::sql() {
    return generate_create_table_sql<regulatory_book_type_entity>(lg());
}

void regulatory_book_type_repository::write(context ctx, const domain::regulatory_book_type& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing regulatory book type: " << v.code;
    execute_write_query(ctx,
                        regulatory_book_type_mapper::map(v),
                        lg(),
                        "Writing regulatory book type to database.");
}

void regulatory_book_type_repository::write(context ctx,
                                            const std::vector<domain::regulatory_book_type>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing regulatory book types. Count: " << v.size();
    execute_write_query(ctx,
                        regulatory_book_type_mapper::map(v),
                        lg(),
                        "Writing regulatory book types to database.");
}

std::vector<domain::regulatory_book_type>
regulatory_book_type_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<regulatory_book_type_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c);

    return execute_read_query<regulatory_book_type_entity, domain::regulatory_book_type>(
        ctx,
        query,
        [](const auto& entities) { return regulatory_book_type_mapper::map(entities); },
        lg(),
        "Reading latest regulatory book types");
}

std::vector<domain::regulatory_book_type>
regulatory_book_type_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest regulatory book type. code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<regulatory_book_type_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<regulatory_book_type_entity, domain::regulatory_book_type>(
        ctx,
        query,
        [](const auto& entities) { return regulatory_book_type_mapper::map(entities); },
        lg(),
        "Reading latest regulatory book type by code.");
}

std::vector<domain::regulatory_book_type>
regulatory_book_type_repository::read_all(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all regulatory book type versions. code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<regulatory_book_type_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code) |
                       order_by("version"_c.desc());

    return execute_read_query<regulatory_book_type_entity, domain::regulatory_book_type>(
        ctx,
        query,
        [](const auto& entities) { return regulatory_book_type_mapper::map(entities); },
        lg(),
        "Reading all regulatory book type versions by code.");
}


void regulatory_book_type_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing regulatory book type: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<regulatory_book_type_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing regulatory book type from database.");
}

std::vector<domain::regulatory_book_type> regulatory_book_type_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest regulatory book types with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<regulatory_book_type_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<regulatory_book_type_entity, domain::regulatory_book_type>(
        ctx,
        query,
        [](const auto& entities) { return regulatory_book_type_mapper::map(entities); },
        lg(),
        "Reading latest regulatory book types with pagination.");
}

std::uint32_t regulatory_book_type_repository::get_total_type_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active regulatory book type count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<regulatory_book_type_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active regulatory book type count: " << count;
    return count;
}

void regulatory_book_type_repository::remove(context ctx, const std::vector<std::string>& codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<regulatory_book_type_entity> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing regulatory book types.");
}


}
