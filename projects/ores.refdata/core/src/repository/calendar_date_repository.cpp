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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_repository.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.refdata.core/repository/calendar_date_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/calendar_date_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/calendar_date_entity.hpp"
#include "ores.refdata.core/repository/calendar_date_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string calendar_date_repository::sql() {
    return generate_create_table_sql<calendar_date_entity>(lg());
}

calendar_date_repository::calendar_date_repository(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::calendar_date> calendar_date_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_date_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("calendar_code"_c, "date"_c);

    return execute_read_query<calendar_date_entity, domain::calendar_date>(
        ctx_,
        query,
        [](const auto& entities) { return calendar_date_mapper::map(entities); },
        lg(),
        "Reading latest calendar dates");
}

std::vector<domain::calendar_date> calendar_date_repository::read_latest(std::uint32_t offset,
                                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar dates with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_date_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("calendar_code"_c, "date"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<calendar_date_entity, domain::calendar_date>(
        ctx_,
        query,
        [](const auto& entities) { return calendar_date_mapper::map(entities); },
        lg(),
        "Reading latest calendar dates (paginated).");
}

std::uint32_t calendar_date_repository::get_total_calendar_date_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active calendar dates count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<calendar_date_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active calendar dates count: " << count;
    return count;
}

std::vector<domain::calendar_date>
calendar_date_repository::read_latest_by_calendar(const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar dates. Calendar: " << calendar_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_date_entity>> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       order_by("date"_c);

    auto rows = execute_read_query<calendar_date_entity, domain::calendar_date>(
        ctx_,
        query,
        [](const auto& entities) { return calendar_date_mapper::map(entities); },
        lg(),
        "Reading latest calendar dates by calendar.");

    return rows;
}

std::vector<domain::calendar_date>
calendar_date_repository::read_latest_by_date(const std::string& date) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar dates. Date: " << date;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<calendar_date_entity>> |
        where("tenant_id"_c == tid && "date"_c == date && "valid_to"_c == max.value()) |
        order_by("calendar_code"_c);

    auto rows = execute_read_query<calendar_date_entity, domain::calendar_date>(
        ctx_,
        query,
        [](const auto& entities) { return calendar_date_mapper::map(entities); },
        lg(),
        "Reading latest calendar dates by date.");

    return rows;
}

std::vector<domain::calendar_date> calendar_date_repository::read_latest_by_calendar(
    const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest calendar dates. Calendar: " << calendar_code
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<calendar_date_entity>> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       order_by("date"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<calendar_date_entity, domain::calendar_date>(
        ctx_,
        query,
        [](const auto& entities) { return calendar_date_mapper::map(entities); },
        lg(),
        "Reading latest calendar dates by calendar (paginated).");

    return rows;
}

std::uint32_t calendar_date_repository::get_total_calendar_date_count_by_calendar(
    const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active calendar dates count. Calendar: "
                               << calendar_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<calendar_date_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active calendar dates count by calendar: " << count;
    return count;
}

std::uint32_t
calendar_date_repository::get_total_calendar_date_count_by_date(const std::string& date) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active calendar dates count. Date: " << date;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<calendar_date_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "date"_c == date && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active calendar dates count by date: " << count;
    return count;
}

}
