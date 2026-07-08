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
#include "ores.synthetic.core/repository/fx_spot_generation_config_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.core/repository/fx_spot_generation_config_entity.hpp"
#include "ores.synthetic.core/repository/fx_spot_generation_config_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::synthetic::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string fx_spot_generation_config_repository::sql() {
    return generate_create_table_sql<fx_spot_generation_config_entity>(lg());
}

void fx_spot_generation_config_repository::write(context ctx,
                                                 const domain::fx_spot_generation_config& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing FX spot generation config: " << v.id;
    execute_write_query(ctx,
                        fx_spot_generation_config_mapper::map(v),
                        lg(),
                        "Writing FX spot generation config to database.");
}

void fx_spot_generation_config_repository::write(
    context ctx, const std::vector<domain::fx_spot_generation_config>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing FX spot generation configs. Count: " << v.size();
    execute_write_query(ctx,
                        fx_spot_generation_config_mapper::map(v),
                        lg(),
                        "Writing FX spot generation configs to database.");
}

std::vector<domain::fx_spot_generation_config>
fx_spot_generation_config_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<fx_spot_generation_config_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<fx_spot_generation_config_entity, domain::fx_spot_generation_config>(
        ctx,
        query,
        [](const auto& entities) { return fx_spot_generation_config_mapper::map(entities); },
        lg(),
        "Reading latest FX spot generation configs");
}

std::vector<domain::fx_spot_generation_config>
fx_spot_generation_config_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest FX spot generation config. id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<fx_spot_generation_config_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<fx_spot_generation_config_entity, domain::fx_spot_generation_config>(
        ctx,
        query,
        [](const auto& entities) { return fx_spot_generation_config_mapper::map(entities); },
        lg(),
        "Reading latest FX spot generation config by id.");
}

std::vector<domain::fx_spot_generation_config>
fx_spot_generation_config_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all FX spot generation config versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<fx_spot_generation_config_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) | order_by("version"_c.desc());

    return execute_read_query<fx_spot_generation_config_entity, domain::fx_spot_generation_config>(
        ctx,
        query,
        [](const auto& entities) { return fx_spot_generation_config_mapper::map(entities); },
        lg(),
        "Reading all FX spot generation config versions by id.");
}


void fx_spot_generation_config_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing FX spot generation config: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<fx_spot_generation_config_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing FX spot generation config from database.");
}

std::vector<domain::fx_spot_generation_config> fx_spot_generation_config_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest FX spot generation configs with offset: "
                               << offset << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<fx_spot_generation_config_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<fx_spot_generation_config_entity, domain::fx_spot_generation_config>(
        ctx,
        query,
        [](const auto& entities) { return fx_spot_generation_config_mapper::map(entities); },
        lg(),
        "Reading latest FX spot generation configs with pagination.");
}

std::uint32_t
fx_spot_generation_config_repository::get_total_fx_spot_generation_config_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active FX spot generation config count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<fx_spot_generation_config_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active FX spot generation config count: " << count;
    return count;
}

void fx_spot_generation_config_repository::remove(context ctx,
                                                  const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<fx_spot_generation_config_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing FX spot generation configs.");
}


}
