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
#include "ores.refdata.core/repository/calendar_rule_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/calendar_rule_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/calendar_rule_entity.hpp"
#include "ores.refdata.core/repository/calendar_rule_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string calendar_rule_repository::sql() {
    return generate_create_table_sql<calendar_rule_entity>(lg());
}

void calendar_rule_repository::write(context ctx, const domain::calendar_rule& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing calendar rule: " << v.id;
    execute_write_query(
        ctx, calendar_rule_mapper::map(v), lg(), "Writing calendar rule to database.");
}

void calendar_rule_repository::write(context ctx, const std::vector<domain::calendar_rule>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing calendar rules. Count: " << v.size();
    execute_write_query(
        ctx, calendar_rule_mapper::map(v), lg(), "Writing calendar rules to database.");
}

std::vector<domain::calendar_rule> calendar_rule_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_rule_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<calendar_rule_entity, domain::calendar_rule>(
        ctx,
        query,
        [](const auto& entities) { return calendar_rule_mapper::map(entities); },
        lg(),
        "Reading latest calendar rules");
}

std::vector<domain::calendar_rule> calendar_rule_repository::read_latest(context ctx,
                                                                         const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar rule. id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_rule_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<calendar_rule_entity, domain::calendar_rule>(
        ctx,
        query,
        [](const auto& entities) { return calendar_rule_mapper::map(entities); },
        lg(),
        "Reading latest calendar rule by id.");
}

std::vector<domain::calendar_rule> calendar_rule_repository::read_all(context ctx,
                                                                      const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all calendar rule versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_rule_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<calendar_rule_entity, domain::calendar_rule>(
        ctx,
        query,
        [](const auto& entities) { return calendar_rule_mapper::map(entities); },
        lg(),
        "Reading all calendar rule versions by id.");
}

std::optional<domain::calendar_rule> calendar_rule_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading calendar rule at version. id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_rule_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<calendar_rule_entity, domain::calendar_rule>(
        ctx,
        query,
        [](const auto& entities) { return calendar_rule_mapper::map(entities); },
        lg(),
        "Reading calendar rule at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::calendar_rule> calendar_rule_repository::read_latest_by_calendar_code(
    context ctx, const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar rules. calendar_code: " << calendar_code
                               << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_rule_entity>> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<calendar_rule_entity, domain::calendar_rule>(
        ctx,
        query,
        [](const auto& entities) { return calendar_rule_mapper::map(entities); },
        lg(),
        "Reading latest calendar rules by calendar_code.");
}

std::uint32_t calendar_rule_repository::get_total_calendar_rule_count_by_calendar_code(
    context ctx, const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active calendar rules count. calendar_code: "
                               << calendar_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<calendar_rule_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active calendar rules count by calendar_code: " << count;
    return count;
}

std::vector<domain::calendar_rule> calendar_rule_repository::read_by_calendar_code_as_of(
    context ctx,
    const std::string& calendar_code,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug) << "Reading calendar rules as of window. calendar_code: "
                               << calendar_code;

    const auto vf(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_from_bound), lg()));
    const auto vt(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_to_bound), lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_rule_entity>> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_from"_c < vt.value() && "valid_to"_c > vf.value()) |
                       order_by("id"_c);

    return execute_read_query<calendar_rule_entity, domain::calendar_rule>(
        ctx,
        query,
        [](const auto& entities) { return calendar_rule_mapper::map(entities); },
        lg(),
        "Reading calendar rules as of window by calendar_code.");
}
void calendar_rule_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing calendar rule: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<calendar_rule_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing calendar rule from database.");
}

std::vector<domain::calendar_rule>
calendar_rule_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar rules with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_rule_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<calendar_rule_entity, domain::calendar_rule>(
        ctx,
        query,
        [](const auto& entities) { return calendar_rule_mapper::map(entities); },
        lg(),
        "Reading latest calendar rules with pagination.");
}

std::uint32_t calendar_rule_repository::get_total_calendar_rule_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active calendar rule count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<calendar_rule_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active calendar rule count: " << count;
    return count;
}

void calendar_rule_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<calendar_rule_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing calendar rules.");
}


}
