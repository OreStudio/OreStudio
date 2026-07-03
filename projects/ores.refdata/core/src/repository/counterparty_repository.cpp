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
#include "ores.refdata.core/repository/counterparty_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/counterparty_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/counterparty_entity.hpp"
#include "ores.refdata.core/repository/counterparty_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string counterparty_repository::sql() {
    return generate_create_table_sql<counterparty_entity>(lg());
}

void counterparty_repository::write(context ctx, const domain::counterparty& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparty: " << v.id;
    execute_write_query(
        ctx, counterparty_mapper::map(v), lg(), "Writing counterparty to database.");
}

void counterparty_repository::write(context ctx, const std::vector<domain::counterparty>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparties. Count: " << v.size();
    execute_write_query(
        ctx, counterparty_mapper::map(v), lg(), "Writing counterparties to database.");
}

std::vector<domain::counterparty> counterparty_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(),
        "Reading latest counterparties");
}

std::vector<domain::counterparty> counterparty_repository::read_latest(context ctx,
                                                                       const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty. id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(),
        "Reading latest counterparty by id.");
}

std::vector<domain::counterparty> counterparty_repository::read_all(context ctx,
                                                                    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all counterparty versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) | order_by("version"_c.desc());

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(),
        "Reading all counterparty versions by id.");
}

void counterparty_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<counterparty_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing counterparty from database.");
}

std::vector<domain::counterparty>
counterparty_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparties with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(),
        "Reading latest counterparties with pagination.");
}

std::uint32_t counterparty_repository::get_total_counterparty_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active counterparty count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<counterparty_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active counterparty count: " << count;
    return count;
}

void counterparty_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<counterparty_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing counterparties.");
}


}
