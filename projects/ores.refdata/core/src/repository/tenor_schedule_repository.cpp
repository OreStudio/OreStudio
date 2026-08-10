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
#include "ores.refdata.core/repository/tenor_schedule_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/tenor_schedule_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/tenor_schedule_entity.hpp"
#include "ores.refdata.core/repository/tenor_schedule_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string tenor_schedule_repository::sql() {
    return generate_create_table_sql<tenor_schedule_entity>(lg());
}

void tenor_schedule_repository::write(context ctx, const domain::tenor_schedule& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenor schedule. " << "code: " << v.code;
    execute_write_query(
        ctx, tenor_schedule_mapper::map(v), lg(), "Writing tenor schedule to database.");
}

void tenor_schedule_repository::write(context ctx, const std::vector<domain::tenor_schedule>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenor schedules. Count: " << v.size();
    execute_write_query(
        ctx, tenor_schedule_mapper::map(v), lg(), "Writing tenor schedules to database.");
}

std::vector<domain::tenor_schedule> tenor_schedule_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_schedule_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c);

    return execute_read_query<tenor_schedule_entity, domain::tenor_schedule>(
        ctx,
        query,
        [](const auto& entities) { return tenor_schedule_mapper::map(entities); },
        lg(),
        "Reading latest tenor schedules");
}

std::vector<domain::tenor_schedule>
tenor_schedule_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor schedule. " << "code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<tenor_schedule_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<tenor_schedule_entity, domain::tenor_schedule>(
        ctx,
        query,
        [](const auto& entities) { return tenor_schedule_mapper::map(entities); },
        lg(),
        "Reading latest tenor schedule by code.");
}


std::vector<domain::tenor_schedule> tenor_schedule_repository::read_all(context ctx,
                                                                        const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all tenor schedule versions. " << "code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_schedule_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<tenor_schedule_entity, domain::tenor_schedule>(
        ctx,
        query,
        [](const auto& entities) { return tenor_schedule_mapper::map(entities); },
        lg(),
        "Reading all tenor schedule versions by code.");
}

std::optional<domain::tenor_schedule> tenor_schedule_repository::read_at_version(
    context ctx, const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading tenor schedule at version. " << "code: " << code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_schedule_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<tenor_schedule_entity, domain::tenor_schedule>(
        ctx,
        query,
        [](const auto& entities) { return tenor_schedule_mapper::map(entities); },
        lg(),
        "Reading tenor schedule at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::tenor_schedule> tenor_schedule_repository::read_latest_by_calendar_code(
    context ctx, const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor schedules. calendar_code: " << calendar_code
                               << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_schedule_entity>> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       order_by("code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<tenor_schedule_entity, domain::tenor_schedule>(
        ctx,
        query,
        [](const auto& entities) { return tenor_schedule_mapper::map(entities); },
        lg(),
        "Reading latest tenor schedules by calendar_code.");
}

std::uint32_t tenor_schedule_repository::get_total_schedule_count_by_calendar_code(
    context ctx, const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active tenor schedules count. calendar_code: "
                               << calendar_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<tenor_schedule_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active tenor schedules count by calendar_code: " << count;
    return count;
}

std::vector<domain::tenor_schedule> tenor_schedule_repository::read_latest_by_diary_entry_type(
    context ctx, const std::string& diary_entry_type, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor schedules. diary_entry_type: "
                               << diary_entry_type << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_schedule_entity>> |
                       where("tenant_id"_c == tid && "diary_entry_type"_c == diary_entry_type &&
                             "valid_to"_c == max.value()) |
                       order_by("code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<tenor_schedule_entity, domain::tenor_schedule>(
        ctx,
        query,
        [](const auto& entities) { return tenor_schedule_mapper::map(entities); },
        lg(),
        "Reading latest tenor schedules by diary_entry_type.");
}

std::uint32_t tenor_schedule_repository::get_total_schedule_count_by_diary_entry_type(
    context ctx, const std::string& diary_entry_type) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active tenor schedules count. diary_entry_type: " << diary_entry_type;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<tenor_schedule_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "diary_entry_type"_c == diary_entry_type &&
                             "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active tenor schedules count by diary_entry_type: "
                               << count;
    return count;
}

void tenor_schedule_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenor schedule. " << "code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<tenor_schedule_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing tenor schedule from database.");
}

std::vector<domain::tenor_schedule>
tenor_schedule_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor schedules with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_schedule_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<tenor_schedule_entity, domain::tenor_schedule>(
        ctx,
        query,
        [](const auto& entities) { return tenor_schedule_mapper::map(entities); },
        lg(),
        "Reading latest tenor schedules with pagination.");
}

std::uint32_t tenor_schedule_repository::get_total_schedule_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active tenor schedule count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<tenor_schedule_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active tenor schedule count: " << count;
    return count;
}

void tenor_schedule_repository::remove(context ctx, const std::vector<std::string>& codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<tenor_schedule_entity> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing tenor schedules.");
}


}
