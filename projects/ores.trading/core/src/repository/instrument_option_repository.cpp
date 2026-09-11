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
#include "ores.trading.core/repository/instrument_option_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/instrument_option_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/instrument_option_entity.hpp"
#include "ores.trading.core/repository/instrument_option_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string instrument_option_repository::sql() {
    return generate_create_table_sql<instrument_option_entity>(lg());
}

void instrument_option_repository::write(context ctx, const domain::instrument_option& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing instrument option. "
                               << "instrument_id: " << v.instrument_id;
    execute_write_query(
        ctx, instrument_option_mapper::map(v), lg(), "Writing instrument option to database.");
}

void instrument_option_repository::write(context ctx,
                                         const std::vector<domain::instrument_option>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing instrument options. Count: " << v.size();
    execute_write_query(
        ctx, instrument_option_mapper::map(v), lg(), "Writing instrument options to database.");
}

std::vector<domain::instrument_option> instrument_option_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_option_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c);

    return execute_read_query<instrument_option_entity, domain::instrument_option>(
        ctx,
        query,
        [](const auto& entities) { return instrument_option_mapper::map(entities); },
        lg(),
        "Reading latest instrument options");
}

std::vector<domain::instrument_option>
instrument_option_repository::read_latest(context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest instrument option. "
                               << "instrument_id: " << instrument_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_option_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
                             "valid_to"_c == max.value());

    return execute_read_query<instrument_option_entity, domain::instrument_option>(
        ctx,
        query,
        [](const auto& entities) { return instrument_option_mapper::map(entities); },
        lg(),
        "Reading latest instrument option by instrument_id.");
}


std::vector<domain::instrument_option>
instrument_option_repository::read_all(context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all instrument option versions. "
                               << "instrument_id: " << instrument_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_option_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<instrument_option_entity, domain::instrument_option>(
        ctx,
        query,
        [](const auto& entities) { return instrument_option_mapper::map(entities); },
        lg(),
        "Reading all instrument option versions by instrument_id.");
}

std::optional<domain::instrument_option> instrument_option_repository::read_at_version(
    context ctx, const std::string& instrument_id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading instrument option at version. "
                               << "instrument_id: " << instrument_id << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_option_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
                             "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<instrument_option_entity, domain::instrument_option>(
        ctx,
        query,
        [](const auto& entities) { return instrument_option_mapper::map(entities); },
        lg(),
        "Reading instrument option at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void instrument_option_repository::remove(context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing instrument option. "
                               << "instrument_id: " << instrument_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<instrument_option_entity> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
                             "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing instrument option from database.");
}

std::vector<domain::instrument_option>
instrument_option_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest instrument options with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_option_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<instrument_option_entity, domain::instrument_option>(
        ctx,
        query,
        [](const auto& entities) { return instrument_option_mapper::map(entities); },
        lg(),
        "Reading latest instrument options with pagination.");
}

std::uint32_t instrument_option_repository::get_total_instrument_option_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active instrument option count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<instrument_option_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active instrument option count: " << count;
    return count;
}

void instrument_option_repository::remove(context ctx,
                                          const std::vector<std::string>& instrument_ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<instrument_option_entity> |
                       where("tenant_id"_c == tid && "instrument_id"_c.in(instrument_ids) &&
                             "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing instrument options.");
}


}
