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
#include "ores.refdata.core/repository/currency_calendar_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/currency_calendar_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/currency_calendar_entity.hpp"
#include "ores.refdata.core/repository/currency_calendar_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string currency_calendar_repository::sql() {
    return generate_create_table_sql<currency_calendar_entity>(lg());
}

currency_calendar_repository::currency_calendar_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void currency_calendar_repository::write(const domain::currency_calendar& currency_calendar) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency calendar to database: "
                               << currency_calendar.currency_iso_code << "/"
                               << currency_calendar.calendar_code;
    execute_write_query(ctx_,
                        currency_calendar_mapper::map(currency_calendar),
                        lg(),
                        "writing currency calendar to database");
}

void currency_calendar_repository::write(
    const std::vector<domain::currency_calendar>& currency_calendars) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency calendars to database. Count: "
                               << currency_calendars.size();
    execute_write_query(ctx_,
                        currency_calendar_mapper::map(currency_calendars),
                        lg(),
                        "writing currency calendars to database");
}

std::vector<domain::currency_calendar> currency_calendar_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_calendar_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("currency_iso_code"_c, "calendar_code"_c);

    return execute_read_query<currency_calendar_entity, domain::currency_calendar>(
        ctx_,
        query,
        [](const auto& entities) { return currency_calendar_mapper::map(entities); },
        lg(),
        "Reading latest currency calendars");
}

std::vector<domain::currency_calendar>
currency_calendar_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency calendars with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_calendar_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("currency_iso_code"_c, "calendar_code"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<currency_calendar_entity, domain::currency_calendar>(
        ctx_,
        query,
        [](const auto& entities) { return currency_calendar_mapper::map(entities); },
        lg(),
        "Reading latest currency calendars (paginated).");
}

std::uint32_t currency_calendar_repository::get_total_currency_calendar_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active currency calendars count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<currency_calendar_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active currency calendars count: " << count;
    return count;
}

std::vector<domain::currency_calendar>
currency_calendar_repository::read_latest_by_currency(const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency calendars. Currency: "
                               << currency_iso_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_calendar_entity>> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "valid_to"_c == max.value()) |
                       order_by("calendar_code"_c);

    auto rows = execute_read_query<currency_calendar_entity, domain::currency_calendar>(
        ctx_,
        query,
        [](const auto& entities) { return currency_calendar_mapper::map(entities); },
        lg(),
        "Reading latest currency calendars by currency.");

    return rows;
}

std::vector<domain::currency_calendar>
currency_calendar_repository::read_latest_by_calendar(const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency calendars. Calendar: " << calendar_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_calendar_entity>> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       order_by("currency_iso_code"_c);

    auto rows = execute_read_query<currency_calendar_entity, domain::currency_calendar>(
        ctx_,
        query,
        [](const auto& entities) { return currency_calendar_mapper::map(entities); },
        lg(),
        "Reading latest currency calendars by calendar.");

    return rows;
}

std::vector<domain::currency_calendar> currency_calendar_repository::read_latest_by_currency(
    const std::string& currency_iso_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency calendars. Currency: "
                               << currency_iso_code << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_calendar_entity>> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "valid_to"_c == max.value()) |
                       order_by("calendar_code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<currency_calendar_entity, domain::currency_calendar>(
        ctx_,
        query,
        [](const auto& entities) { return currency_calendar_mapper::map(entities); },
        lg(),
        "Reading latest currency calendars by currency (paginated).");

    return rows;
}

std::uint32_t currency_calendar_repository::get_total_currency_calendar_count_by_currency(
    const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active currency calendars count. Currency: "
                               << currency_iso_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<currency_calendar_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active currency calendars count by currency: " << count;
    return count;
}

std::uint32_t currency_calendar_repository::get_total_currency_calendar_count_by_calendar(
    const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active currency calendars count. Calendar: "
                               << calendar_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<currency_calendar_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active currency calendars count by calendar: " << count;
    return count;
}

void currency_calendar_repository::remove(const std::string& currency_iso_code,
                                          const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency calendar from database: " << currency_iso_code
                               << "/" << calendar_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<currency_calendar_entity> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "calendar_code"_c == calendar_code);

    execute_delete_query(ctx_, query, lg(), "removing currency calendar from database");
}

void currency_calendar_repository::remove_by_currency(const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all currency calendars from database: "
                               << currency_iso_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<currency_calendar_entity> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code);

    execute_delete_query(ctx_, query, lg(), "removing all currency calendars from database");
}

}
